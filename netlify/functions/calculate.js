// netlify/functions/calculate.js

const CONFIG = {
  transferTariff04: 0.329,
  transferTariff10: 0.265,
  transformerCostPerMW: 6e6,
  heatPowerPerUnit: 600,
  heatUsageFactor: 0.65,
  fotYear: 810000 * 12,
  MAINTENANCE_SCHEDULE: [
    { interval: 1000, cost: 556024 },
    { interval: 4000, cost: 4416 },
    { interval: 8000, cost: 122900 },
    { interval: 16000, cost: 1882677 },
    { interval: 48000, cost: 822371 }
  ]
};

function irr(cfs, guess = 0.1) {
  let r = guess;
  for (let i = 0; i < 200; i++) {
    let npv = 0, dnpv = 0;
    for (let t = 0; t < cfs.length; t++) {
      const f = Math.pow(1 + r, t);
      npv += cfs[t] / f;
      if (t > 0) dnpv += (-t * cfs[t]) / Math.pow(1 + r, t + 1);
    }
    if (Math.abs(dnpv) < 1e-12) return null;
    const nr = r - npv / dnpv;
    if (Math.abs(nr - r) < 1e-8) return nr;
    r = nr;
  }
  return null;
}

function buildProjectModel(params) {
  const years = Math.max(1, params.projectYears);
  const staged = params.stagedCommissioning?.active || false;
  let installedPowerByYear = new Array(years + 1).fill(0);
  let totalCapexByYear = new Array(years + 1).fill(0);
  let totalCapexSum = 0;

  if (staged) {
    if (params.stagedCommissioning.preProjectCapex) {
      totalCapexByYear[0] = params.stagedCommissioning.preProjectCapex;
      totalCapexSum += totalCapexByYear[0];
    }
    const stages = params.stagedCommissioning.stages || [];
    let capexPerMW = (params.capexEquipment || 80) + (params.capexDesign || 10) + (params.capexNetwork || 10) + (params.capexCommissioning || 10) + (params.capexInfrastructure || 5.5);
    if (params.voltageKV === 0.4) capexPerMW += 6;
    for (let s of stages) {
      const year = s.year;
      if (year < 1 || year > years) continue;
      const powerStep = s.powerMW;
      const stageCapex = powerStep * capexPerMW * 1e6;
      totalCapexByYear[year] += stageCapex;
      totalCapexSum += stageCapex;
      for (let y = year; y <= years; y++) installedPowerByYear[y] += powerStep;
    }
    if (stages.length === 0 && params.powerMW) {
      installedPowerByYear.fill(params.powerMW, 1);
      const capexTotal = params.powerMW * capexPerMW * 1e6;
      totalCapexByYear[0] = capexTotal;
      totalCapexSum = capexTotal;
    }
  } else {
    let capexPerMW = (params.capexEquipment || 80) + (params.capexDesign || 10) + (params.capexNetwork || 10) + (params.capexCommissioning || 10) + (params.capexInfrastructure || 5.5);
    if (params.voltageKV === 0.4) capexPerMW += 6;
    let capexTotal = params.powerMW * capexPerMW * 1e6;
    if (params.capexPerMWTotal) capexTotal = params.powerMW * params.capexPerMWTotal * 1e6;
    const profile = params.capexProfile && params.capexProfile.length ? params.capexProfile : [{ yearOffset: 0, percent: 100 }];
    const totalPercent = profile.reduce((s, p) => s + p.percent, 0);
    const norm = totalPercent > 0 ? 100 / totalPercent : 1;
    for (let p of profile) {
      if (p.yearOffset <= years) totalCapexByYear[p.yearOffset] += capexTotal * (p.percent * norm / 100);
    }
    totalCapexSum = totalCapexByYear.reduce((a, b) => a + b, 0);
    for (let y = 1; y <= years; y++) installedPowerByYear[y] = params.powerMW;
  }

  const getPower = (t) => t === 0 ? 0 : installedPowerByYear[t];
  const getUnits = (t) => t === 0 ? 0 : Math.ceil(getPower(t) / 0.5);

  let deprBaseByYear = new Array(years + 1).fill(0);
  let cumulativeDeprBase = 0;
  const amortizablePercent = params.taxes.amortizableCapexPercent !== undefined ? params.taxes.amortizableCapexPercent : 85;
  for (let y = 0; y <= years; y++) {
    if (y === 0 && totalCapexByYear[0] > 0) {
      cumulativeDeprBase += totalCapexByYear[0] * (amortizablePercent / 100);
    } else if (y >= 1) {
      cumulativeDeprBase += totalCapexByYear[y] * (amortizablePercent / 100);
    }
    deprBaseByYear[y] = cumulativeDeprBase;
  }
  const deprAnnualRate = params.taxes.depreciationYears > 0 ? 1 / params.taxes.depreciationYears : 0;
  const getDepr = (t) => t === 0 ? 0 : deprBaseByYear[t] * deprAnnualRate;

  const debtActive = params.debt.active;
  const debtShare = debtActive ? params.debt.share / 100 : 0;
  const equityShare = 1 - debtShare;
  const debtAmount = debtActive ? totalCapexSum * debtShare : 0;
  const rate = debtActive ? params.debt.interestRate / 100 : 0;
  const term = debtActive ? Math.min(params.debt.term, years) : 0;
  const grace = debtActive ? params.debt.graceYears : 0;
  const type = debtActive ? params.debt.paymentType : 'annuity';
  let remaining = debtAmount;
  const debtSchedule = [];
  if (debtActive && term > 0) {
    let ann = 0;
    const payYears = term - grace;
    if (type === 'annuity' && rate > 0 && payYears > 0) ann = remaining * (rate * Math.pow(1 + rate, payYears)) / (Math.pow(1 + rate, payYears) - 1);
    for (let t = 1; t <= term; t++) {
      let interest = remaining * rate;
      let principal = 0, payment = interest;
      if (t > grace) {
        if (type === 'annuity') principal = Math.min(ann - interest, remaining);
        else principal = payYears > 0 ? Math.min(debtAmount / payYears, remaining) : 0;
        payment = interest + principal;
      }
      if (t === term && remaining > 0) { principal = remaining; payment = interest + principal; }
      debtSchedule.push({ year: t, debt: remaining, interest, principal, payment });
      remaining -= principal;
      if (remaining < 0) remaining = 0;
    }
  }
  const debtDraw = new Array(years + 1).fill(0);
  for (let t = 0; t <= years; t++) debtDraw[t] = debtActive ? totalCapexByYear[t] * debtShare : 0;

  const infl = params.inflation;
  const getInfl = (r, t) => Math.pow(1 + (r || 0) / 100, t - 1);
  const transferTariff = params.voltageKV === 0.4 ? CONFIG.transferTariff04 : CONFIG.transferTariff10;
  const vatRate = params.taxes.useVAT ? params.taxes.vatRate / 100 : 0;
  const includeVatInCapex = params.taxes.vatOnCapex || false;

  const oilActive = params.oil ? params.oil.active : false;
  const oilRate = oilActive ? params.oil.consumptionRate : 0;
  const oilPrice = oilActive ? params.oil.price : 0;
  const oilChangeInterval = oilActive ? params.oil.changeInterval : 2000;
  const oilChangeCost = oilActive ? params.oil.changeCost : 0;
  const oilTopUpCost = oilActive ? params.oil.topUpCost : 0;
  const wasteActive = params.waste ? params.waste.active : false;
  const wasteType = wasteActive ? params.waste.type : 'fixed';
  const wasteFixed = wasteActive ? params.waste.fixedCost : 0;
  const wasteRate = wasteActive ? params.waste.ratePerHour : 0;
  const consumablesActive = params.consumables ? params.consumables.active : false;
  const consumablesAnnual = consumablesActive ? params.consumables.totalAnnual : 0;

  let maintAccum = {};
  CONFIG.MAINTENANCE_SCHEDULE.forEach(m => maintAccum[m.interval] = 0);

  const yearlyOp = [];
  for (let t = 1; t <= years; t++) {
    const P_inst = getPower(t);
    if (P_inst === 0) {
      yearlyOp.push({ year: t, totalRevenue_excl_vat: 0, revenueGrid_excl_vat: 0, revenueHeat_excl_vat: 0, opex_excl_vat: 0, ebitda: 0, depr: 0, propertyTax: 0, vat_net: 0, fuelCost: 0, maintCost: 0, fotCost: 0, ecoCost: 0, otherOpex: 0, totalOilCost: 0, wasteCost: 0, consumablesCost: 0, profitTax: 0, netKWh: 0 });
      continue;
    }
    const units = getUnits(t);
    const nominalHours = params.operatingHours;
    let effectiveHours = nominalHours;
    if (params.availability.active) effectiveHours *= (params.availability.rate / 100);
    let grossKWh = 0, ownUseKWh = 0;
    if (params.multimode.active) {
      let shares = [...params.multimode.timeShares];
      const sum = shares.reduce((a, b) => a + b, 0);
      if (sum > 0) shares = shares.map(s => s / sum); else shares = [0.33, 0.33, 0.34];
      const loads = params.multimode.loadFactors;
      for (let i = 0; i < 3; i++) {
        const h = effectiveHours * shares[i];
        const g = P_inst * 1000 * h * loads[i];
        grossKWh += g;
        ownUseKWh += g * (params.ownNeeds.percent / 100);
      }
    } else {
      grossKWh = P_inst * 1000 * effectiveHours * params.loadFactor;
      ownUseKWh = grossKWh * (params.ownNeeds.percent / 100);
    }
    const netKWh = grossKWh - ownUseKWh;
    let baseCons = params.gasConsumption;
    if (params.efficiencyCorrection.active) {
      const load = params.loadFactor;
      const corr = params.efficiencyCorrection.percent / 100;
      let add = load < 0.5 ? corr : (load < 1 ? corr * (1 - load) / 0.5 : 0);
      baseCons *= (1 + add);
    }
    const deg = Math.pow(1 + params.degradationPerYear / 100, t - 1);
    let gasYearlyM3 = grossKWh * baseCons * deg;
    if (params.startupGas.active) {
      gasYearlyM3 += params.startupGas.startupsPerUnit * params.startupGas.gasPerStartup * units;
    }
    let maintYearly = 0;
    for (let m of CONFIG.MAINTENANCE_SCHEDULE) {
      maintAccum[m.interval] += effectiveHours;
      const cnt = Math.floor(maintAccum[m.interval] / m.interval);
      if (cnt > 0) {
        maintYearly += cnt * m.cost * units;
        maintAccum[m.interval] -= cnt * m.interval;
      }
    }
    let fuelCost = gasYearlyM3 * params.fuelCostRubM3 * getInfl(infl.gas, t);
    let maintCost = maintYearly * getInfl(infl.maint, t);
    let fotCost = CONFIG.fotYear * getInfl(infl.fot, t);
    let ecoCost = (params.ecologyBase * params.ecologyFactor) * getInfl(infl.eco, t);
    let otherOpex = 0;
    if (params.otherOpex.active) {
      otherOpex = params.otherOpex.totalAnnual * getInfl(params.otherOpex.inflationType === 'general' ? (infl.general || 0) : 0, t);
      if (params.otherOpex.includeCapacityPayment) otherOpex += params.otherOpex.capacityPaymentRate * P_inst * 1000 * 12 * getInfl(infl.general, t);
    }
    const oilVolume = units * (effectiveHours * oilRate / 1000);
    const oilCostTotal = oilVolume * oilPrice * getInfl(infl.general, t);
    const oilChanges = Math.floor(effectiveHours / oilChangeInterval);
    const oilChangeTotal = units * oilChanges * oilChangeCost * getInfl(infl.general, t);
    const oilTopUpTotal = units * oilTopUpCost * getInfl(infl.general, t);
    const totalOilCost = oilCostTotal + oilChangeTotal + oilTopUpTotal;
    let wasteCost = 0;
    if (wasteActive) {
      if (wasteType === 'fixed') wasteCost = wasteFixed * getInfl(infl.general, t);
      else wasteCost = effectiveHours * wasteRate * getInfl(infl.general, t);
    }
    const consumablesCost = consumablesAnnual * getInfl(infl.general, t);
    let opex_excl_vat = fuelCost + maintCost + fotCost + ecoCost + otherOpex + totalOilCost + wasteCost + consumablesCost;
    let revenueGrid_excl_vat = 0;
    if (params.multimode.active) {
      let shares = [...params.multimode.timeShares];
      const sum = shares.reduce((a, b) => a + b, 0);
      if (sum > 0) shares = shares.map(s => s / sum);
      const tariffs = params.multimode.tariffs;
      const loads = params.multimode.loadFactors;
      for (let i = 0; i < 3; i++) {
        const h = effectiveHours * shares[i];
        const gross = P_inst * 1000 * h * loads[i];
        const net = gross * (1 - params.ownNeeds.percent / 100);
        revenueGrid_excl_vat += net * (tariffs[i] + transferTariff) * getInfl(infl.grid, t);
      }
    } else {
      const net = grossKWh * (1 - params.ownNeeds.percent / 100);
      revenueGrid_excl_vat = net * (params.gridPriceRubKWh + transferTariff) * getInfl(infl.grid, t);
    }
    let revenueHeat_excl_vat = 0;
    if (params.cogeneration.active) {
      const heatMWh = units * CONFIG.heatPowerPerUnit * effectiveHours * CONFIG.heatUsageFactor / 1000;
      revenueHeat_excl_vat = heatMWh * 0.859845 * params.cogeneration.heatTariffRubGcal * getInfl(infl.heat, t);
    }
    const totalRevenue_excl_vat = revenueGrid_excl_vat + revenueHeat_excl_vat;
    const ebitda = totalRevenue_excl_vat - opex_excl_vat;
    let vat_out = totalRevenue_excl_vat * vatRate;
    let vat_in_opex = opex_excl_vat * vatRate;
    let vat_in_capex = 0;
    if (includeVatInCapex && totalCapexByYear[t]) vat_in_capex = totalCapexByYear[t] * vatRate;
    let vat_net = vat_out - vat_in_opex - vat_in_capex;
    let propertyTax = 0;
    if (params.taxes.useCadastral) propertyTax = params.taxes.cadastralBuildingsValue * (params.taxes.cadastralRate / 100);
    else if (params.taxes.usePropertyTax) {
      const residual = Math.max(0, deprBaseByYear[t] - getDepr(t) * t);
      propertyTax = residual * (params.taxes.propertyTaxRate / 100);
    }
    const depr = getDepr(t);
    yearlyOp.push({ year: t, totalRevenue_excl_vat, revenueGrid_excl_vat, revenueHeat_excl_vat, opex_excl_vat, ebitda, depr, propertyTax, vat_net, fuelCost, maintCost, fotCost, ecoCost, otherOpex, totalOilCost, wasteCost, consumablesCost, netKWh });
  }

  const profitTaxCorrected = new Array(years).fill(0);
  let lossCarryForward = 0;
  for (let i = 0; i < years; i++) {
    const interest = (debtSchedule.length > i) ? debtSchedule[i].interest : 0;
    let ebit = yearlyOp[i].ebitda - yearlyOp[i].depr - yearlyOp[i].propertyTax - interest;
    let tax = 0;
    if (ebit > 0 && lossCarryForward > 0) { const used = Math.min(ebit, lossCarryForward); ebit -= used; lossCarryForward -= used; }
    if (ebit > 0 && params.taxes.useProfitTax && !(params.taxes.taxHoliday && (i + 1) <= params.taxes.taxHolidayYears)) tax = ebit * (params.taxes.profitTaxRate / 100);
    else if (ebit < 0) lossCarryForward += -ebit;
    profitTaxCorrected[i] = tax;
    yearlyOp[i].profitTax = tax;
  }
  const unleveredProfitTax = new Array(years).fill(0);
  let unleveredLossCF = 0;
  for (let i = 0; i < years; i++) {
    let ebit = yearlyOp[i].ebitda - yearlyOp[i].depr - yearlyOp[i].propertyTax;
    let tax = 0;
    if (ebit > 0 && unleveredLossCF > 0) { const used = Math.min(ebit, unleveredLossCF); ebit -= used; unleveredLossCF -= used; }
    if (ebit > 0 && params.taxes.useProfitTax && !(params.taxes.taxHoliday && (i + 1) <= params.taxes.taxHolidayYears)) tax = ebit * (params.taxes.profitTaxRate / 100);
    else if (ebit < 0) unleveredLossCF += -ebit;
    unleveredProfitTax[i] = tax;
  }

  const maintCapexAnnual = (params.maintCapexPercent / 100) * totalCapexSum;
  const cfads = new Array(years).fill(0);
  for (let i = 0; i < years; i++) {
    let cf = yearlyOp[i].ebitda - profitTaxCorrected[i] - yearlyOp[i].propertyTax - maintCapexAnnual;
    if (params.workingCapital.active) {
      if (i === 0) cf -= yearlyOp[i].totalRevenue_excl_vat * (params.workingCapital.percent / 100);
      if (i + 1 === years) cf += yearlyOp[i].totalRevenue_excl_vat * (params.workingCapital.percent / 100);
    }
    if (i + 1 === years) cf += totalCapexSum * (params.salvagePercent / 100);
    cf -= yearlyOp[i].vat_net;
    cfads[i] = cf;
  }

  const fcff = new Array(years).fill(0);
  for (let i = 0; i < years; i++) {
    let growthCapex = totalCapexByYear[i + 1];
    let wcChange = 0;
    if (params.workingCapital.active) {
      if (i === 0) wcChange = -yearlyOp[i].totalRevenue_excl_vat * (params.workingCapital.percent / 100);
      if (i + 1 === years) wcChange = yearlyOp[i].totalRevenue_excl_vat * (params.workingCapital.percent / 100);
    }
    let salvageAfterTax = (i + 1 === years) ? totalCapexSum * (params.salvagePercent / 100) : 0;
    fcff[i] = yearlyOp[i].ebitda - unleveredProfitTax[i] - yearlyOp[i].propertyTax + yearlyOp[i].depr - maintCapexAnnual - growthCapex + wcChange + salvageAfterTax;
  }

  const equity_cf = new Array(years).fill(0);
  const equityInvestmentByYear = new Array(years + 1).fill(0);
  for (let t = 0; t <= years; t++) equityInvestmentByYear[t] = totalCapexByYear[t] * equityShare;
  for (let i = 0; i < years; i++) {
    const debtService = (debtSchedule.length > i) ? debtSchedule[i].payment : 0;
    const draw = debtDraw[i + 1];
    const equityInv = equityInvestmentByYear[i + 1];
    equity_cf[i] = cfads[i] + draw - debtService - equityInv;
  }
  const totalEquityInvestment = equityInvestmentByYear.reduce((a, b) => a + b, 0);

  const taxRateProfit = params.taxes.useProfitTax ? params.taxes.profitTaxRate / 100 : 0;
  const kd_after_tax = debtActive ? rate * (1 - taxRateProfit) : 0;
  const ke = (params.wacc.riskFree / 100) + (params.wacc.beta * params.wacc.equityRisk / 100);
  const wacc = equityShare * ke + debtShare * kd_after_tax;
  const llcrDiscountRate = params.debt.llcrDiscountRate ? params.debt.llcrDiscountRate / 100 : rate;

  let project_npv = -totalCapexByYear[0];
  const project_irr_cfs = [-totalCapexByYear[0]];
  for (let t = 0; t < years; t++) { project_npv += fcff[t] / Math.pow(1 + wacc, t + 1); project_irr_cfs.push(fcff[t]); }
  const project_irr = irr(project_irr_cfs);

  let equity_npv = -equityInvestmentByYear[0];
  const equity_irr_cfs = [-equityInvestmentByYear[0]];
  for (let t = 0; t < years; t++) { equity_npv += equity_cf[t] / Math.pow(1 + ke, t + 1); equity_irr_cfs.push(equity_cf[t]); }
  const equity_irr = irr(equity_irr_cfs);

  const dscr_vals = [];
  for (let i = 0; i < years; i++) { const ds = (debtSchedule.length > i) ? debtSchedule[i].payment : 0; if (ds > 1e-3) dscr_vals.push(cfads[i] / ds); }
  const min_dscr = dscr_vals.length ? Math.min(...dscr_vals) : Infinity;
  let pv_cfads_llcr = 0;
  for (let i = 0; i < Math.min(term, years); i++) pv_cfads_llcr += cfads[i] / Math.pow(1 + llcrDiscountRate, i + 1);
  const llcr = debtAmount > 0 ? pv_cfads_llcr / debtAmount : null;

  return {
    yearly_operating: yearlyOp,
    cfads,
    fcff,
    equity_cf,
    debtSchedule,
    capexSchedule: totalCapexByYear,
    metrics: {
      project_npv, project_irr, equity_npv, equity_irr,
      min_dscr, avg_dscr: dscr_vals.length ? dscr_vals.reduce((a, b) => a + b, 0) / dscr_vals.length : Infinity,
      llcr, roe: totalEquityInvestment > 0 ? (yearlyOp.reduce((s, y) => s + y.ebitda - y.depr - y.propertyTax - (debtSchedule.length > y.year - 1 ? debtSchedule[y.year - 1].interest : 0) - y.profitTax, 0) / years) / totalEquityInvestment * 100 : 0,
      wacc, ke, llcrDiscountRate,
      total_investment: totalCapexSum, total_equity_investment: totalEquityInvestment,
      dscr_values: dscr_vals
    }
  };
}

exports.handler = async (event) => {
  if (event.httpMethod !== 'POST') {
    return { statusCode: 405, body: 'Method Not Allowed' };
  }
  try {
    const params = JSON.parse(event.body);
    const model = buildProjectModel(params);
    return {
      statusCode: 200,
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(model)
    };
  } catch (err) {
    console.error(err);
    return {
      statusCode: 500,
      body: JSON.stringify({ error: err.message })
    };
  }
};