# The VJA model: every equation written out, as one koma equation string.
#
# Sourced by scratch/vja_model_build.R, which builds the panel
# (scratch/vja_model_panel.R), checks this file against it, estimates and
# forecasts. Nothing here is generated: edit the string to change the model.
#
# Entities: Germany (de), France (fr), Italy (it), the rest of the euro area
# (reu), the United States (us), the United Kingdom (gb), Japan (jp), China
# (cn) and India (in). The euro area shares one short rate and one exchange
# rate (ea_). All series are quarterly levels; koma estimates on growth rates,
# except for interest rates, which it takes as they are.
#
# Equations, for each of de, fr, it, reu, us, gb and jp:
#   consumption, investment  own lag, the long rate
#   goods exports            foreign demand, the exchange rate
#   services exports         foreign demand
#   goods, services imports  domestic demand
#   prices                   own lag, the exchange rate (not Japan, whose
#                            CPI starts only in 2010)
#   long rate                own lag, the short rate, the benchmark yield
#                            (Bund for the euro area, Treasury elsewhere)
# For ea, us, gb and jp, the currency's
#   short rate               own lag, inflation, GDP growth (Taylor rule;
#                            Japan without inflation)
#   exchange rate            own and US short rate, now and a quarter ago
#                            (the euro area for the US)
# For cn and in: GDP (own lag, exports), exports (foreign demand) and imports
# (GDP).
#
# Identities: exports and imports are goods plus services; GDP is domestic
# demand plus exports less imports, and domestic demand is consumption,
# investment and government consumption (exogenous); foreign demand is the
# trade-weighted imports of the other eight entities; ea_gdp and ea_prices
# aggregate the four euro-area entities.
#
# The rules koma imposes and does not check (CLAUDE.md):
# - Every stochastic equation comes before every identity.
# - A negative identity term is written "- (a/b)*x", never "+ -(a/b)*x".
# - No lagged variable's name is a prefix of another's.
#
# Identity weights are written as koma's injected weights, (a/b)*a: koma
# takes annual sums of a and b, shifts them back a year and keeps the value
# at dates$dynamic_weights$end, so each weight is one number, the ratio in
# 2025, for the whole sample and the forecast (docs/koma-api.md, 2.2). A
# weight can only be the ratio of two series: koma does not parse a sum in
# the denominator, so every denominator is a series of its own.
# - exports, imports, gdp, ea_gdp: the model's own volumes, in the national
#   currency (euro for the euro area). GDP's weights sum to 0.99-1.00, the
#   rest being inventories and the statistical discrepancy.
# - domestic_demand: has no published series; the panel builds it so that its
#   2025 level is consumption plus investment plus government consumption.
# - foreign_demand: dots_<from>_<to> is IMF DOTS goods exports from one
#   entity to another, dots_<from>_partners the sum over the other eight. The
#   eight take between a third (US) and two thirds (France) of each entity's
#   exports; the rest is assumed to grow like the part they take.
# - ea_prices: <cc>_nominal_consumption is nominal household consumption
#   (Eurostat), the basis of the HICP's country weights. A price index cannot
#   weight itself.
# The weight series are in the panel (scratch/vja_model_panel.R) but are not
# model variables: koma reads them only to evaluate the weights.
#
# `[tau = 2.2]` doubles koma's default Metropolis step for the equations whose
# acceptance rate came out just above the 20-60% band at the default 1.1.

vja_equations <- "
de_consumption ~ constant + de_consumption.L(1) + de_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
de_investment ~ constant + de_investment.L(1) + de_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
de_goods_exports ~ constant + de_foreign_demand + ea_neer + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
de_services_exports ~ constant + de_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
de_goods_imports ~ constant + de_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
de_services_imports ~ constant + de_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
de_prices ~ constant + de_prices.L(1) + ea_neer
de_long_rate ~ constant + de_long_rate.L(1) + ea_short_rate + us_long_rate

fr_consumption ~ constant + fr_consumption.L(1) + fr_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
fr_investment ~ constant + fr_investment.L(1) + fr_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
fr_goods_exports ~ constant + fr_foreign_demand + ea_neer + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
fr_services_exports ~ constant + fr_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
fr_goods_imports ~ constant + fr_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
fr_services_imports ~ constant + fr_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
fr_prices ~ constant + fr_prices.L(1) + ea_neer
fr_long_rate ~ constant + fr_long_rate.L(1) + ea_short_rate + de_long_rate

it_consumption ~ constant + it_consumption.L(1) + it_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
it_investment ~ constant + it_investment.L(1) + it_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
it_goods_exports ~ constant + it_foreign_demand + ea_neer + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
it_services_exports ~ constant + it_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
it_goods_imports ~ constant + it_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
it_services_imports ~ constant + it_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
it_prices ~ constant + it_prices.L(1) + ea_neer
it_long_rate ~ constant + it_long_rate.L(1) + ea_short_rate + de_long_rate

reu_consumption ~ constant + reu_consumption.L(1) + reu_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
reu_investment ~ constant + reu_investment.L(1) + reu_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
reu_goods_exports ~ constant + reu_foreign_demand + ea_neer + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
reu_services_exports ~ constant + reu_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
reu_goods_imports ~ constant + reu_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
reu_services_imports ~ constant + reu_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
reu_prices ~ constant + reu_prices.L(1) + ea_neer [tau = 2.2]
reu_long_rate ~ constant + reu_long_rate.L(1) + ea_short_rate + de_long_rate

ea_short_rate ~ constant + ea_short_rate.L(1) + ea_prices + ea_gdp
ea_neer ~ constant + ea_short_rate + ea_short_rate.L(1) + us_short_rate + us_short_rate.L(1)

us_consumption ~ constant + us_consumption.L(1) + us_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
us_investment ~ constant + us_investment.L(1) + us_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
us_goods_exports ~ constant + us_foreign_demand + us_neer + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
us_services_exports ~ constant + us_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
us_goods_imports ~ constant + us_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
us_services_imports ~ constant + us_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
us_prices ~ constant + us_prices.L(1) + us_neer
us_long_rate ~ constant + us_long_rate.L(1) + us_short_rate [tau = 2.2]
us_short_rate ~ constant + us_short_rate.L(1) + us_prices + us_gdp
us_neer ~ constant + us_short_rate + us_short_rate.L(1) + ea_short_rate + ea_short_rate.L(1)

gb_consumption ~ constant + gb_consumption.L(1) + gb_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
gb_investment ~ constant + gb_investment.L(1) + gb_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
gb_goods_exports ~ constant + gb_foreign_demand + gb_neer + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
gb_services_exports ~ constant + gb_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
gb_goods_imports ~ constant + gb_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
gb_services_imports ~ constant + gb_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
gb_prices ~ constant + gb_prices.L(1) + gb_neer [tau = 2.2]
gb_long_rate ~ constant + gb_long_rate.L(1) + gb_short_rate + us_long_rate
gb_short_rate ~ constant + gb_short_rate.L(1) + gb_prices + gb_gdp
gb_neer ~ constant + gb_short_rate + gb_short_rate.L(1) + us_short_rate + us_short_rate.L(1)

jp_consumption ~ constant + jp_consumption.L(1) + jp_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
jp_investment ~ constant + jp_investment.L(1) + jp_long_rate + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
jp_goods_exports ~ constant + jp_foreign_demand + jp_neer + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
jp_services_exports ~ constant + jp_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
jp_goods_imports ~ constant + jp_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
jp_services_imports ~ constant + jp_domestic_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
jp_long_rate ~ constant + jp_long_rate.L(1) + jp_short_rate + us_long_rate
jp_short_rate ~ constant + jp_short_rate.L(1) + jp_gdp
jp_neer ~ constant + jp_short_rate + jp_short_rate.L(1) + us_short_rate + us_short_rate.L(1)

cn_gdp ~ constant + cn_gdp.L(1) + cn_exports + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
cn_exports ~ constant + cn_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
cn_imports ~ constant + cn_gdp + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]

in_gdp ~ constant + in_gdp.L(1) + in_exports + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2
in_exports ~ constant + in_foreign_demand + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]
in_imports ~ constant + in_gdp + covid_2020q1 + covid_2020q2 + covid_2020q3 + covid_2021q2 [tau = 2.2]

de_exports == (de_goods_exports/de_exports)*de_goods_exports + (de_services_exports/de_exports)*de_services_exports
de_imports == (de_goods_imports/de_imports)*de_goods_imports + (de_services_imports/de_imports)*de_services_imports
de_domestic_demand == (de_consumption/de_domestic_demand)*de_consumption + (de_investment/de_domestic_demand)*de_investment + (de_government/de_domestic_demand)*de_government
de_gdp == (de_domestic_demand/de_gdp)*de_domestic_demand + (de_exports/de_gdp)*de_exports - (de_imports/de_gdp)*de_imports

fr_exports == (fr_goods_exports/fr_exports)*fr_goods_exports + (fr_services_exports/fr_exports)*fr_services_exports
fr_imports == (fr_goods_imports/fr_imports)*fr_goods_imports + (fr_services_imports/fr_imports)*fr_services_imports
fr_domestic_demand == (fr_consumption/fr_domestic_demand)*fr_consumption + (fr_investment/fr_domestic_demand)*fr_investment + (fr_government/fr_domestic_demand)*fr_government
fr_gdp == (fr_domestic_demand/fr_gdp)*fr_domestic_demand + (fr_exports/fr_gdp)*fr_exports - (fr_imports/fr_gdp)*fr_imports

it_exports == (it_goods_exports/it_exports)*it_goods_exports + (it_services_exports/it_exports)*it_services_exports
it_imports == (it_goods_imports/it_imports)*it_goods_imports + (it_services_imports/it_imports)*it_services_imports
it_domestic_demand == (it_consumption/it_domestic_demand)*it_consumption + (it_investment/it_domestic_demand)*it_investment + (it_government/it_domestic_demand)*it_government
it_gdp == (it_domestic_demand/it_gdp)*it_domestic_demand + (it_exports/it_gdp)*it_exports - (it_imports/it_gdp)*it_imports

reu_exports == (reu_goods_exports/reu_exports)*reu_goods_exports + (reu_services_exports/reu_exports)*reu_services_exports
reu_imports == (reu_goods_imports/reu_imports)*reu_goods_imports + (reu_services_imports/reu_imports)*reu_services_imports
reu_domestic_demand == (reu_consumption/reu_domestic_demand)*reu_consumption + (reu_investment/reu_domestic_demand)*reu_investment + (reu_government/reu_domestic_demand)*reu_government
reu_gdp == (reu_domestic_demand/reu_gdp)*reu_domestic_demand + (reu_exports/reu_gdp)*reu_exports - (reu_imports/reu_gdp)*reu_imports

us_exports == (us_goods_exports/us_exports)*us_goods_exports + (us_services_exports/us_exports)*us_services_exports
us_imports == (us_goods_imports/us_imports)*us_goods_imports + (us_services_imports/us_imports)*us_services_imports
us_domestic_demand == (us_consumption/us_domestic_demand)*us_consumption + (us_investment/us_domestic_demand)*us_investment + (us_government/us_domestic_demand)*us_government
us_gdp == (us_domestic_demand/us_gdp)*us_domestic_demand + (us_exports/us_gdp)*us_exports - (us_imports/us_gdp)*us_imports

gb_exports == (gb_goods_exports/gb_exports)*gb_goods_exports + (gb_services_exports/gb_exports)*gb_services_exports
gb_imports == (gb_goods_imports/gb_imports)*gb_goods_imports + (gb_services_imports/gb_imports)*gb_services_imports
gb_domestic_demand == (gb_consumption/gb_domestic_demand)*gb_consumption + (gb_investment/gb_domestic_demand)*gb_investment + (gb_government/gb_domestic_demand)*gb_government
gb_gdp == (gb_domestic_demand/gb_gdp)*gb_domestic_demand + (gb_exports/gb_gdp)*gb_exports - (gb_imports/gb_gdp)*gb_imports

jp_exports == (jp_goods_exports/jp_exports)*jp_goods_exports + (jp_services_exports/jp_exports)*jp_services_exports
jp_imports == (jp_goods_imports/jp_imports)*jp_goods_imports + (jp_services_imports/jp_imports)*jp_services_imports
jp_domestic_demand == (jp_consumption/jp_domestic_demand)*jp_consumption + (jp_investment/jp_domestic_demand)*jp_investment + (jp_government/jp_domestic_demand)*jp_government
jp_gdp == (jp_domestic_demand/jp_gdp)*jp_domestic_demand + (jp_exports/jp_gdp)*jp_exports - (jp_imports/jp_gdp)*jp_imports

de_foreign_demand == (dots_de_fr/dots_de_partners)*fr_imports + (dots_de_it/dots_de_partners)*it_imports + (dots_de_reu/dots_de_partners)*reu_imports + (dots_de_us/dots_de_partners)*us_imports + (dots_de_gb/dots_de_partners)*gb_imports + (dots_de_jp/dots_de_partners)*jp_imports + (dots_de_cn/dots_de_partners)*cn_imports + (dots_de_in/dots_de_partners)*in_imports
fr_foreign_demand == (dots_fr_de/dots_fr_partners)*de_imports + (dots_fr_it/dots_fr_partners)*it_imports + (dots_fr_reu/dots_fr_partners)*reu_imports + (dots_fr_us/dots_fr_partners)*us_imports + (dots_fr_gb/dots_fr_partners)*gb_imports + (dots_fr_jp/dots_fr_partners)*jp_imports + (dots_fr_cn/dots_fr_partners)*cn_imports + (dots_fr_in/dots_fr_partners)*in_imports
it_foreign_demand == (dots_it_de/dots_it_partners)*de_imports + (dots_it_fr/dots_it_partners)*fr_imports + (dots_it_reu/dots_it_partners)*reu_imports + (dots_it_us/dots_it_partners)*us_imports + (dots_it_gb/dots_it_partners)*gb_imports + (dots_it_jp/dots_it_partners)*jp_imports + (dots_it_cn/dots_it_partners)*cn_imports + (dots_it_in/dots_it_partners)*in_imports
reu_foreign_demand == (dots_reu_de/dots_reu_partners)*de_imports + (dots_reu_fr/dots_reu_partners)*fr_imports + (dots_reu_it/dots_reu_partners)*it_imports + (dots_reu_us/dots_reu_partners)*us_imports + (dots_reu_gb/dots_reu_partners)*gb_imports + (dots_reu_jp/dots_reu_partners)*jp_imports + (dots_reu_cn/dots_reu_partners)*cn_imports + (dots_reu_in/dots_reu_partners)*in_imports
us_foreign_demand == (dots_us_de/dots_us_partners)*de_imports + (dots_us_fr/dots_us_partners)*fr_imports + (dots_us_it/dots_us_partners)*it_imports + (dots_us_reu/dots_us_partners)*reu_imports + (dots_us_gb/dots_us_partners)*gb_imports + (dots_us_jp/dots_us_partners)*jp_imports + (dots_us_cn/dots_us_partners)*cn_imports + (dots_us_in/dots_us_partners)*in_imports
gb_foreign_demand == (dots_gb_de/dots_gb_partners)*de_imports + (dots_gb_fr/dots_gb_partners)*fr_imports + (dots_gb_it/dots_gb_partners)*it_imports + (dots_gb_reu/dots_gb_partners)*reu_imports + (dots_gb_us/dots_gb_partners)*us_imports + (dots_gb_jp/dots_gb_partners)*jp_imports + (dots_gb_cn/dots_gb_partners)*cn_imports + (dots_gb_in/dots_gb_partners)*in_imports
jp_foreign_demand == (dots_jp_de/dots_jp_partners)*de_imports + (dots_jp_fr/dots_jp_partners)*fr_imports + (dots_jp_it/dots_jp_partners)*it_imports + (dots_jp_reu/dots_jp_partners)*reu_imports + (dots_jp_us/dots_jp_partners)*us_imports + (dots_jp_gb/dots_jp_partners)*gb_imports + (dots_jp_cn/dots_jp_partners)*cn_imports + (dots_jp_in/dots_jp_partners)*in_imports
cn_foreign_demand == (dots_cn_de/dots_cn_partners)*de_imports + (dots_cn_fr/dots_cn_partners)*fr_imports + (dots_cn_it/dots_cn_partners)*it_imports + (dots_cn_reu/dots_cn_partners)*reu_imports + (dots_cn_us/dots_cn_partners)*us_imports + (dots_cn_gb/dots_cn_partners)*gb_imports + (dots_cn_jp/dots_cn_partners)*jp_imports + (dots_cn_in/dots_cn_partners)*in_imports
in_foreign_demand == (dots_in_de/dots_in_partners)*de_imports + (dots_in_fr/dots_in_partners)*fr_imports + (dots_in_it/dots_in_partners)*it_imports + (dots_in_reu/dots_in_partners)*reu_imports + (dots_in_us/dots_in_partners)*us_imports + (dots_in_gb/dots_in_partners)*gb_imports + (dots_in_jp/dots_in_partners)*jp_imports + (dots_in_cn/dots_in_partners)*cn_imports

ea_gdp == (de_gdp/ea_gdp)*de_gdp + (fr_gdp/ea_gdp)*fr_gdp + (it_gdp/ea_gdp)*it_gdp + (reu_gdp/ea_gdp)*reu_gdp
ea_prices == (de_nominal_consumption/ea_nominal_consumption)*de_prices + (fr_nominal_consumption/ea_nominal_consumption)*fr_prices + (it_nominal_consumption/ea_nominal_consumption)*it_prices + (reu_nominal_consumption/ea_nominal_consumption)*reu_prices
"

# Government consumption has no equation. The COVID quarters enter the
# real-side equations as 0/1 dummies: those whose mean absolute GDP growth
# across the nine entities is more than three times its 2004Q3-2019Q4 average
# of 0.93pp (2020Q1 3.8 times, 2020Q2 15.3, 2020Q3 12.0, 2021Q2 3.5; next is
# 2021Q3 at 2.7).
vja_exogenous <- c(
  "de_government", "fr_government", "it_government", "reu_government",
  "us_government", "gb_government", "jp_government",
  "covid_2020q1", "covid_2020q2", "covid_2020q3", "covid_2021q2"
)

# The sample starts where India's and China's series do (2004Q2 levels, so
# koma's first growth rate and lag leave 2004Q4) and ends with the last
# national accounts published everywhere. Forecasts run eight quarters. The
# identity weights are the 2025 ratios: koma uses the year before
# dynamic_weights$end.
vja_dates <- list(
  estimation = list(start = c(2004, 2), end = c(2026, 2)),
  forecast = list(start = c(2026, 3), end = c(2028, 2)),
  dynamic_weights = list(start = c(2026, 1), end = c(2026, 2))
)
