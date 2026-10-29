const fs = require("fs");
const d = require("docx");
const { Document, Packer, Paragraph, TextRun, HeadingLevel, ImageRun, Table, TableRow, TableCell, WidthType,
        AlignmentType, BorderStyle, PageBreak, LineNumberRestartFormat } = d;
const ROOT = "/home/user/grazing_inverts_meta-analysis/";
const FONT = "Times New Roman";

// inline markup: *italic*, ^superscript^, ~subscript~
function runs(text, base = {}) {
  const out = [];
  const re = /(\*[^*]+\*|\^[^^]+\^|~[^~]+~)/g;
  let last = 0, m;
  while ((m = re.exec(text)) !== null) {
    if (m.index > last) out.push(new TextRun({ text: text.slice(last, m.index), ...base }));
    const t = m[0], inner = t.slice(1, -1);
    if (t[0] === "*") out.push(new TextRun({ text: inner, italics: true, ...base }));
    if (t[0] === "^") out.push(new TextRun({ text: inner, superScript: true, ...base }));
    if (t[0] === "~") out.push(new TextRun({ text: inner, subScript: true, ...base }));
    last = m.index + t.length;
  }
  if (last < text.length) out.push(new TextRun({ text: text.slice(last), ...base }));
  return out;
}
const P = (t, o = {}) => new Paragraph({ children: runs(t, o.run || {}), spacing: { after: 120, line: 480 },
                                          alignment: o.align || AlignmentType.JUSTIFIED, indent: o.indent });
const H1 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_1, children: [new TextRun(t)], spacing: { before: 360, after: 120 } });
const H2 = (t) => new Paragraph({ heading: HeadingLevel.HEADING_2, children: [new TextRun(t)], spacing: { before: 240, after: 120 } });
const NOTE = (t) => new Paragraph({ children: [new TextRun({ text: t, italics: true, color: "C00000" })], spacing: { after: 120 } });

// ---------------------------------------------------------------- tables
const thin = { style: BorderStyle.SINGLE, size: 4, color: "000000" };
const none = { style: BorderStyle.NONE, size: 0, color: "FFFFFF" };
function table(header, rows, widths, notes) {
  const total = widths.reduce((a, b) => a + b, 0);
  const cell = (t, i, isHead, isLast) => new TableCell({
    width: { size: widths[i], type: WidthType.DXA },
    borders: { top: isHead ? thin : none, bottom: (isHead || isLast) ? thin : none, left: none, right: none },
    margins: { top: 40, bottom: 40, left: 80, right: 80 },
    children: [new Paragraph({ alignment: i === 0 ? AlignmentType.LEFT : AlignmentType.CENTER,
                               children: runs(t, { size: 18, bold: isHead && false, font: FONT }) })] });
  const tRows = [new TableRow({ tableHeader: true, children: header.map((h, i) => cell(h, i, true, false)) })]
    .concat(rows.map((r, j) => new TableRow({ children: r.map((c, i) => cell(c, i, false, j === rows.length - 1)) })));
  const out = [new Table({ width: { size: total, type: WidthType.DXA }, columnWidths: widths, rows: tRows })];
  if (notes) out.push(new Paragraph({ children: runs(notes, { size: 18 }), spacing: { before: 80, after: 240 } }));
  return out;
}

// ---------------------------------------------------------------- figures
const FIGS = [
  ["Fig1_study_sites", 105, "**Figure 1** Locations of the 79 studies (52 locations) included in the meta-analysis. Point area is proportional to the number of effect sizes from each site; colour shows whether native, domestic or both/mixed grazers were excluded. Robinson projection."],
  ["Fig2_overall_stratum", 120, "**Figure 2** Effects of grazer exclusion on invertebrate abundance, richness and diversity (a) overall and (b) by invertebrate stratum. Small points are individual effect sizes (size proportional to precision, 1/SE); large points and bars are model estimates and 95% confidence intervals from multilevel meta-analytic models. The upper axis gives the equivalent percentage change in exclosures relative to grazed plots. Numbers give effect sizes (studies). Positive values indicate higher values where grazers were excluded."],
  ["Fig3_grazer_size", 140, "**Figure 3** Effects of grazer exclusion by grazer body size: (a) the largest size class excluded (Small < 10 kg, Medium 10–100 kg, Large 100–1000 kg, Mega ≥ 1000 kg) and (b) exclosures that removed a single size class only. Symbols as in Figure 2. Classes represented by fewer than three studies are not shown."],
  ["Fig4_origin_time", 120, "**Figure 4** Effects of grazer exclusion (a) by the origin of the excluded grazers (native vs. domestic) and (b) in relation to time since exclusion. In (b), points are effect sizes (size proportional to precision), lines and shaded bands are meta-regression fits and 95% confidence intervals; the x-axis is ln(years + 1), labelled in years."],
  ["Fig5_trophic_level", 165, "**Figure 5** Effects of grazer exclusion on invertebrate trophic groups (a) across all exclosures and (b) separately for exclosures excluding native (orange) and domestic (blue) grazers. Symbols as in Figure 2. Trophic groups represented by fewer than three studies within a response (or within an origin in b) are not shown."],
  ["Fig6_biome_plant_biomass", 150, "**Figure 6** (a) Effects of grazer exclusion by biome (RESOLVE Ecoregions 2017). (b) Relationship between invertebrate and above-ground plant-biomass responses to grazer exclusion in studies that measured both; points are study-level mean invertebrate effect sizes (size proportional to precision), lines and bands are meta-regression fits and 95% confidence intervals."],
  ["FigS1_funnel", 65, "**Figure S1** Funnel plots of effect sizes against precision (1/SE, log scale). Vertical solid lines show the overall estimate, shaded regions its expected 95% range given each precision, dashed lines zero."],
];
function figure(name, hmm, caption, first) {
  const w = 602, h = Math.round(w * hmm / 183);
  return [
    ...(first ? [] : [new Paragraph({ children: [new PageBreak()] })]),
    new Paragraph({ alignment: AlignmentType.CENTER, children: [new ImageRun({ type: "png",
      data: fs.readFileSync(ROOT + "output/figures/" + name + ".png"), transformation: { width: w, height: h } })] }),
    new Paragraph({ children: caption.split("**").map((t, i) => i % 2 ? new TextRun({ text: t, bold: true }) : runs(t)).flat(),
                    spacing: { before: 160, line: 360 }, alignment: AlignmentType.JUSTIFIED }),
  ];
}

// ---------------------------------------------------------------- text
const C = [];
C.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 240 },
  children: [new TextRun({ text: "Grazer exclusion and invertebrate communities: a global meta-analysis", bold: true, size: 32 })] }));
C.push(new Paragraph({ alignment: AlignmentType.CENTER, spacing: { after: 360 },
  children: [new TextRun({ text: "Methods and Results (draft)", italics: true })] }));

C.push(H1("2 | MATERIALS AND METHODS"));
C.push(H2("2.1 | Literature search and study selection"));
C.push(NOTE("[To be completed by the authors: databases searched, search string, date of search, inclusion criteria, screening procedure and PRISMA flow diagram.]"));
C.push(P("Studies were included if they reported an invertebrate response (abundance or density, taxonomic richness, or a diversity index) in plots from which vertebrate grazers had been excluded and in paired plots where grazing continued, together with a measure of variance and sample size for both groups. Most comparisons came from fenced exclosure experiments; a minority compared naturally ungrazed with grazed areas (hereafter ‘natural grazed vs. ungrazed’ designs) and were retained but examined in a sensitivity analysis."));

C.push(H2("2.2 | Data extraction and coding"));
C.push(P("For each comparison we extracted the mean, variance (SD or SE) and sample size of the response under exclusion and under grazing, together with study location (latitude, longitude), the grazers excluded, the year exclusion began, the sampling date(s) and the invertebrate taxon, trophic group and stratum. Every sampling year reported by a study was retained as a separate effect size. Studies whose sites lay within 100 km of one another were assigned to a common location (complete-linkage clustering) to account for spatial non-independence."));
C.push(P("The extracted data were checked and cleaned before analysis, and every change was logged. Errors arising from spreadsheet auto-fill were reset to the source value; an exclusion year stored as a date serial was corrected; and taxonomic order and trophic-group labels were standardised (e.g. misspellings corrected, families entered as orders moved to their order, synonymous trophic labels merged). Where a group mean was zero (two studies), a constant equal to half the smallest non-zero mean in that study was added to both group means. Where a non-zero mean had a reported SE of zero (two studies), the SE was imputed from the study’s mean coefficient of variation. Fourteen comparisons lacking a sample size, variance or interpretable response type were excluded."));
C.push(P("Responses were grouped as *abundance* (abundance and density), *richness* (taxonomic richness) and *diversity* (Shannon and other diversity indices) and analysed separately. Moderators were coded as follows. *Stratum*: above-ground (vegetation-dwelling), ground and litter-dwelling, or below-ground (soil). *Grazer size*: the body-mass class of the excluded grazers (Small < 10 kg, Medium 10–100 kg, Large 100–1000 kg, Mega ≥ 1000 kg), coded as the largest class excluded; where several classes were excluded together, the comparison was assigned to the largest. *Grazer origin*: native (wild) or domestic (livestock); comparisons excluding mixed assemblages (*k* = 19) or invasive grazers alone (*k* = 3) were omitted from origin analyses. *Time since exclusion*: years between the start of exclusion and sampling (ranges were represented by their midpoint, open-ended values by their bound), analysed as ln(years + 1). *Trophic group*: herbivore, pollinator, omnivore, predator, parasitoid, detritivore, bacterivore, fungivore, scavenger or algivore, as reported or inferred from taxonomy; comparisons reporting whole communities (‘total’), mixed guilds (herbivore/detritivore) or no trophic information were excluded from trophic analyses."));
C.push(P("*Biome* was assigned by intersecting each site with the RESOLVE Ecoregions 2017 map (Dinerstein et al., 2017). Sites falling just outside a polygon (coastal sites, coordinate rounding; six sites, all within 7 km) were assigned to the nearest ecoregion. Thirteen studies that did not report coordinates were located from their site descriptions; for two of these (one in Spain without a named site, one spanning 16 stands across Sweden) the assignment is approximate. *Continent* was assigned from the study country (Greenland was assigned to North America)."));

C.push(H2("2.3 | Effect size"));
C.push(P("Effects were expressed as the natural log response ratio, lnRR = ln(*X̄*~E~ / *X̄*~G~), where *X̄*~E~ and *X̄*~G~ are the mean response in exclosures and grazed plots respectively (Hedges et al., 1999). Positive values indicate higher values where grazers were excluded. Standard errors were converted to standard deviations (SD = SE × √*n*) before computing lnRR and its sampling variance with the *escalc* function (measure ‘ROM’) of *metafor*. For presentation, estimates were back-transformed to percentage change, 100 × (e^lnRR^ − 1)."));

C.push(H2("2.4 | Statistical analyses"));
C.push(P("All models were multilevel random-effects meta-analyses fitted by restricted maximum likelihood with *rma.mv* in *metafor* v4.4 (Viechtbauer, 2010) in R (R Core Team, 2024). Random intercepts were included for location, study nested within location, and effect size nested within study. Where several comparisons within a study shared the same exclosure or grazed group, the resulting sampling covariance (the shared group’s contribution, SD^2^ / (*n* × *X̄*^2^)) was included in the variance–covariance matrix (Lajeunesse, 2011). Tests and confidence intervals used *t*-distributions with containment degrees of freedom. Because the number of locations was modest and effect sizes were strongly clustered, we also report cluster-robust (CR2) standard errors, confidence intervals and Wald tests, clustered by location, with small-sample corrections (Pustejovsky & Tipton, 2018; *clubSandwich*). We regard effects as robust only when they are supported by both approaches."));
C.push(P("Heterogeneity was quantified with Cochran’s *Q* and the multilevel *I*^2^, partitioned among locations, studies and effect sizes (Nakagawa & Santos, 2012). Each moderator was tested in a separate model within each response. For categorical moderators we report level-specific estimates from a model without an intercept, and an omnibus *F*-test of differences among levels from the equivalent model with an intercept. Levels represented by fewer than three studies were excluded from that model. Grazer size was additionally tested as a linear trend across ordered classes, and in a sensitivity analysis restricted to exclosures that removed a single size class. Time since exclusion was analysed as a continuous moderator."));
C.push(P("To test whether trophic groups responded differently to the exclusion of native and domestic grazers, we (i) fitted trophic group as a moderator separately within native and within domestic exclusions, and (ii) fitted a joint model with trophic group, origin and their interaction, restricted to trophic groups represented by at least three studies under both origins (abundance: herbivores, predators, detritivores; richness: herbivores, predators). From the joint model we report the omnibus test of the interaction and, for each trophic group, the domestic − native contrast."));
C.push(P("To test whether invertebrate responses tracked changes in vegetation, plant-biomass lnRRs reported by the same studies were averaged to one value per study (inverse-variance weighted, assuming a correlation of 0.5 among a study’s plant effects) and used as a moderator of invertebrate lnRR. Only above-ground plant biomass was reported by enough studies (≥ 5) to be analysed."));
C.push(P("Robustness was assessed by (i) comparing exclosure experiments with natural grazed vs. ungrazed comparisons; (ii) refitting the overall models without comparisons in which a zero mean had been adjusted; (iii) leave-one-study-out refits of the overall models; and (iv) a multilevel Egger-type regression with the effect-size standard error as a moderator, whose intercept provides a small-study-adjusted estimate (Nakagawa et al., 2022), alongside funnel plots. Continent was examined in an additional exploratory analysis, with and without adjustment for grazer origin. Figures were produced with *ggplot2* and follow the orchard-plot format (Nakagawa et al., 2021). Code and data are available at [repository URL]."));

// ---------------------------------------------------------------- results
C.push(H1("3 | RESULTS"));
C.push(H2("3.1 | Dataset"));
C.push(P("The final dataset comprised 578 effect sizes from 79 studies at 52 locations in 22 countries, published between 1992 and 2026 (Figure 1). Of these, 417 effect sizes (68 studies, 44 locations) concerned abundance, 115 (35 studies, 31 locations) richness and 46 (17 studies, 16 locations) diversity. Most comparisons came from exclosure experiments (*k* = 533; natural grazed vs. ungrazed comparisons, *k* = 45). Exclusion had been in place for 0–54 years (median 4 years). Comparisons involved the exclusion of native grazers (*k* = 318), domestic grazers (*k* = 238), mixed assemblages (*k* = 19) or invasive grazers (*k* = 3). Studies were concentrated in Europe (32 studies), Asia (18), North America (17) and Africa (10), with two studies from Australia and none from South America (Figure 1)."));

C.push(H2("3.2 | Overall effects of grazer exclusion"));
C.push(P("Excluding grazers increased invertebrate abundance by 14.8% (lnRR = 0.138, 95% CI: 0.011 to 0.265, *p* = .034; robust *p* = .025) and taxonomic richness by 24.1% (lnRR = 0.216, 95% CI: 0.038 to 0.395, *p* = .019; robust *p* = .019), but had no detectable effect on diversity (+10.3%, 95% CI: −15.9% to +44.8%, *p* = .45; Figure 2a; Table 1). Heterogeneity was very high for all responses (total *I*^2^ = 96.6%, 95.6% and 99.6% for abundance, richness and diversity; all *Q* tests *p* < .001). For abundance, heterogeneity was partitioned between studies (*I*^2^ = 53.9%) and among effect sizes within studies (42.7%), whereas for richness and diversity it lay almost entirely among locations (91.9% and 89.4%). Leave-one-study-out estimates ranged from 0.098 to 0.157 for abundance, 0.159 to 0.238 for richness and 0.008 to 0.150 for diversity, so no single study determined the overall results."));

C.push(H2("3.3 | Stratum"));
C.push(P("Responses did not differ significantly among strata for abundance (*F*~2,413~ = 1.05, *p* = .35), richness (*F*~2,112~ = 0.81, *p* = .45) or diversity (*F*~2,14~ = 0.50, *p* = .62; Figure 2b; Table 1). Nevertheless, the increase in abundance was concentrated in above-ground invertebrates (+22.6%, 95% CI: +5.1% to +43.0%, *p* = .010; robust *p* = .024), whereas ground- and litter-dwelling (+8.6%, *p* = .34) and below-ground invertebrates (+9.8%, *p* = .48) showed no significant change. Richness increased in above-ground (+26.6%, 95% CI: +2.3% to +56.7%, *p* = .031; robust *p* = .051) and ground- and litter-dwelling assemblages (+24.9%, 95% CI: +2.3% to +52.4%, *p* = .029; robust *p* = .032), but not below ground (+15.0%, *p* = .22)."));

C.push(H2("3.4 | Grazer size"));
C.push(P("Abundance responses differed among the size classes of excluded grazers in the model-based test (*F*~3,369~ = 9.14, *p* < .001), but not in the cluster-robust test (robust *p* = .38; Figure 3a). The difference was driven by a large increase following the exclusion of small grazers (+276%, 95% CI: +124% to +533%), estimated from only eight effect sizes from three studies at two locations. Excluding medium-sized (−3.2%, *p* = .76) or mega-herbivores (−7.8%, *p* = .68) had no detectable effect on abundance, whereas excluding large grazers increased abundance marginally (+22.5%, 95% CI: +0.3% to +49.6%, *p* = .047; robust *p* = .12). There was no linear trend in abundance responses across size classes (slope = −0.047 per class, *p* = .54). Excluding large grazers increased richness (+36.3%, 95% CI: +9.7% to +69.4%, *p* = .006; robust *p* = .043), but richness responses did not differ significantly between medium and large grazers (*F*~1,92~ = 2.24, *p* = .14). Diversity did not vary with grazer size (*F*~1,34~ = 1.51, *p* = .23; linear trend *p* = .086). When the analysis was restricted to exclosures removing a single size class (Figure 3b), the effect of excluding large grazers on abundance was smaller and non-significant (+10.6%, *p* = .31), indicating that part of the effect attributed to large grazers arose from exclosures that also removed smaller herbivores."));

C.push(H2("3.5 | Grazer origin and time since exclusion"));
C.push(P("Responses did not differ between the exclusion of native and domestic grazers for abundance (*F*~1,394~ = 0.14, *p* = .71), richness (*F*~1,32~ = 0.17, *p* = .69) or diversity (*F*~1,15~ = 0.50, *p* = .49; Figure 4a). Excluding native grazers increased richness (+28.2%, 95% CI: +3.1% to +59.3%, *p* = .026; robust *p* = .044) and tended to increase abundance (+15.8%, *p* = .078); the corresponding effects of excluding domestic grazers were of similar magnitude but not significant (richness +21.9%, *p* = .092; abundance +10.8%, *p* = .28)."));
C.push(P("Abundance responses tended to decline with time since exclusion (slope = −0.108 per unit ln[years + 1], 95% CI: −0.226 to 0.011, *p* = .074; robust *p* = .39), from an estimated +41% at the start of exclusion to close to zero after several decades (Figure 4b). Time since exclusion was unrelated to richness (slope = 0.075, *p* = .38) or diversity responses (slope = −0.012, *p* = .93)."));

C.push(H2("3.6 | Trophic groups"));
C.push(P("Trophic groups did not differ significantly in their abundance responses to grazer exclusion (*F*~7,320~ = 1.27, *p* = .27; Figure 5a; Table 2). Point estimates were positive for most groups and largest for parasitoids (+51.0%, 95% CI: +2.1% to +123.1%, *p* = .039; four studies), bacterivorous nematodes (+37.6%, *p* = .097) and detritivores (+35.8%, 95% CI: +4.3% to +76.9%, *p* = .023), intermediate for predators (+18.5%, *p* = .13) and herbivores (+12.8%, *p* = .26), and negative only for pollinators (−13.0%, *p* = .53). None of the group-level effects was significant in cluster-robust tests (all robust *p* ≥ .17). Richness (*F*~2,66~ = 0.42, *p* = .66) and diversity (*F*~1,13~ = 0.02, *p* = .90) responses likewise did not differ among trophic groups."));
C.push(P("Within origins, trophic groups did not differ in their abundance responses to the exclusion of either native (*F*~4,162~ = 0.51, *p* = .73) or domestic grazers (*F*~5,115~ = 1.49, *p* = .20; Figure 5b). However, in the joint model, the effect of grazer origin on abundance depended on trophic group (trophic group × origin: *F*~2,229~ = 6.63, *p* = .002), although this interaction was not supported by the cluster-robust test (robust *F* = 2.46, *p* = .19). Herbivore abundance increased when native grazers were excluded (+34.0%, 95% CI: +3.3% to +73.9%, *p* = .028) but not when domestic grazers were excluded (−2.5%, *p* = .86; domestic − native contrast = −0.319, 95% CI: −0.674 to 0.036, *p* = .078). Conversely, detritivore abundance increased strongly when domestic grazers were excluded (+84.2%, 95% CI: +23.3% to +175.4%, *p* = .003) but not when native grazers were excluded (+22.3%, *p* = .20; contrast = 0.409, 95% CI: −0.076 to 0.894, *p* = .098), and predator abundance tended to increase following the exclusion of domestic (+33.9%, *p* = .059; robust *p* = .035) rather than native grazers (+14.1%, *p* = .33). For richness, the interaction was not significant (*F*~1,11~ = 0.73, *p* = .41), although herbivore richness increased modestly when domestic grazers were excluded (+14.1%, 95% CI: +3.4% to +25.9%, *p* = .010; robust *p* = .082)."));

C.push(H2("3.7 | Biome, continent and plant biomass"));
C.push(P("Responses to grazer exclusion did not differ among biomes for abundance (*F*~6,33~ = 1.21, *p* = .33), richness (*F*~4,20~ = 0.36, *p* = .83) or diversity (*F*~3,8~ = 0.93, *p* = .47; Figure 6a). Abundance increased in temperate conifer forests (+59.9%, 95% CI: +10.4% to +131.5%, *p* = .015; 15 studies at seven locations), although not in the cluster-robust test (robust *p* = .19), and was the only response to decline, non-significantly, in tropical savannas (−20.0%, *p* = .35). Similarly, responses did not differ among continents for abundance (*F*~3,38~ = 0.40, *p* = .75), richness (*F*~3,26~ = 0.27, *p* = .85) or diversity (*F*~2,12~ = 0.01, *p* = .99), including after adjusting for grazer origin (abundance: *F*~3,35~ = 0.60, *p* = .62). Point estimates for abundance were largest in North America (+29.2%, *p* = .063) and smallest in Africa (+2.9%, *p* = .87)."));
C.push(P("Among studies that measured both, invertebrate responses were unrelated to the response of above-ground plant biomass for abundance (slope = −0.066, 95% CI: −0.571 to 0.440, *p* = .78; 14 studies) and richness (slope = 0.053, 95% CI: −0.287 to 0.393, *p* = .72; eight studies; Figure 6b)."));

C.push(H2("3.8 | Sensitivity analyses and publication bias"));
C.push(P("Effects on abundance were positive in exclosure experiments (+17.8%, 95% CI: +2.9% to +34.9%, *p* = .019) but not in natural grazed vs. ungrazed comparisons (−5.2%, *p* = .77), although the two designs did not differ significantly (*F*~1,42~ = 1.26, *p* = .27); design did not affect richness (*p* = .39) or diversity (*p* = .12). Excluding comparisons with zero-adjusted means did not alter the overall abundance estimate (+14.0%, 95% CI: +0.9% to +28.9%). The multilevel Egger test revealed strong small-study asymmetry for abundance (slope on SE = 0.757, 95% CI: 0.449 to 1.066, *p* < .001; robust *p* = .003): less precise studies reported larger positive effects (Figure S1). The bias-adjusted intercept for abundance was close to zero (−8.3%, 95% CI: −21.4% to +7.1%, *p* = .27), indicating that the overall increase in abundance should be interpreted with caution. There was no evidence of small-study effects for richness (*p* = .12) or diversity (*p* = .47)."));

// ---------------------------------------------------------------- tables
C.push(new Paragraph({ children: [new PageBreak()] }));
C.push(new Paragraph({ children: runs("TABLE 1 Overall effects of grazer exclusion and omnibus tests of moderators for each response. Estimates are percentage change (95% CI) in exclosures relative to grazed plots. Moderator tests are model-based *F*-tests with containment degrees of freedom; robust *p* values are from cluster-robust (CR2) Wald tests clustered by location."), spacing: { after: 120 } }));
C.push(...table(
  ["", "Abundance", "Richness", "Diversity"],
  [["Overall effect, % (95% CI)", "+14.8 (+1.1, +30.3)", "+24.1 (+3.9, +48.4)", "+10.3 (−15.9, +44.8)"],
   ["*p* (robust *p*)", ".034 (.025)", ".019 (.019)", ".45 (.45)"],
   ["*k* / studies / locations", "417 / 68 / 44", "115 / 35 / 31", "46 / 17 / 16"],
   ["Total *I*^2^ (%)", "96.6", "95.6", "99.6"],
   ["Stratum", "*F*~2,413~ = 1.05, *p* = .35 (.35)", "*F*~2,112~ = 0.81, *p* = .45 (.37)", "*F*~2,14~ = 0.50, *p* = .62 (.34)"],
   ["Grazer size (largest class)", "*F*~3,369~ = 9.14, *p* < .001 (.38)", "*F*~1,92~ = 2.24, *p* = .14 (.31)", "*F*~1,34~ = 1.51, *p* = .23 (.48)"],
   ["Grazer size (single class)", "*F*~3,291~ = 6.83, *p* < .001 (.35)", "*F*~1,75~ = 0.31, *p* = .58 (.61)", "*F*~1,24~ = 0.01, *p* = .92 (.90)"],
   ["Grazer size (linear trend)", "*p* = .54 (.71)", "*p* = .98 (.99)", "*p* = .086 (.16)"],
   ["Grazer origin", "*F*~1,394~ = 0.14, *p* = .71 (.69)", "*F*~1,32~ = 0.17, *p* = .69 (.78)", "*F*~1,15~ = 0.50, *p* = .49 (.41)"],
   ["Time since exclusion", "*p* = .074 (.39)", "*p* = .38 (.24)", "*p* = .93 (.85)"],
   ["Trophic group", "*F*~7,320~ = 1.27, *p* = .27 (–)", "*F*~2,66~ = 0.42, *p* = .66 (.79)", "*F*~1,13~ = 0.02, *p* = .90 (.83)"],
   ["Trophic group × origin", "*F*~2,229~ = 6.63, *p* = .002 (.19)", "*F*~1,11~ = 0.73, *p* = .41 (.47)", "–"],
   ["Biome", "*F*~6,33~ = 1.21, *p* = .33 (.93)", "*F*~4,20~ = 0.36, *p* = .83 (.16)", "*F*~3,8~ = 0.93, *p* = .47 (.82)"],
   ["Continent", "*F*~3,38~ = 0.40, *p* = .75 (.90)", "*F*~3,26~ = 0.27, *p* = .85 (.77)", "*F*~2,12~ = 0.01, *p* = .99 (.99)"],
   ["Plant biomass (slope)", "*p* = .78 (.75)", "*p* = .72 (.47)", "–"],
   ["Study design", "*F*~1,42~ = 1.26, *p* = .27 (.28)", "*F*~1,33~ = 0.77, *p* = .39 (.42)", "*F*~1,15~ = 2.68, *p* = .12 (.18)"],
   ["Egger test (SE slope)", "*p* < .001 (.003)", "*p* = .12 (.086)", "*p* = .47 (.70)"]],
  [2700, 2250, 2250, 2160],
  "*Note*: Robust omnibus test for trophic group (abundance) could not be computed because the number of levels was large relative to the number of locations. –, not estimable (insufficient studies)."));

C.push(new Paragraph({ children: runs("TABLE 2 Effects of grazer exclusion on the abundance of invertebrate trophic groups, across all exclosures and separately for the exclusion of native and domestic grazers. Values are percentage change (95% CI); *k* (studies) in brackets. Groups with fewer than three studies are not estimated (–)."), spacing: { before: 240, after: 120 } }));
C.push(...table(
  ["Trophic group", "All exclosures", "Native grazers excluded", "Domestic grazers excluded"],
  [["Herbivore", "+12.8 (−8.6, +39.1) [119 (35)]", "+26.4 (−3.3, +65.3) [69 (15)]", "−5.5 (−27.3, +22.8) [46 (20)]"],
   ["Pollinator", "−13.0 (−43.6, +34.2) [19 (6)]", "+27.3 (−21.5, +106.6) [10 (5)]", "–"],
   ["Omnivore", "+3.9 (−25.5, +44.8) [26 (5)]", "+13.1 (−21.8, +63.4) [18 (4)]", "–"],
   ["Predator", "+18.5 (−5.0, +47.8) [77 (24)]", "+14.5 (−12.5, +49.7) [44 (13)]", "+26.9 (−6.9, +72.9) [32 (12)]"],
   ["Parasitoid", "+51.0 (+2.1, +123.1) [16 (4)]", "–", "+23.7 (−23.7, +100.7) [13 (3)]"],
   ["Detritivore", "+35.8 (+4.3, +76.9) [47 (14)]", "+22.6 (−9.0, +65.3) [26 (9)]", "+67.6 (+7.0, +162.6) [18 (5)]"],
   ["Bacterivore", "+37.6 (−5.6, +100.5) [12 (4)]", "–", "+13.2 (−38.0, +106.6) [6 (3)]"],
   ["Fungivore", "+9.2 (−23.2, +55.2) [12 (4)]", "–", "+0.9 (−45.1, +85.5) [6 (3)]"]],
  [1700, 2560, 2550, 2550],
  "*Note*: Within-origin estimates come from separate models fitted to native and to domestic exclusions; estimates in the text for the joint trophic group × origin model differ slightly because that model shares variance components across origins."));

C.push(H1("References"));
[
 "Dinerstein, E., Olson, D., Joshi, A., Vynne, C., Burgess, N. D., Wikramanayake, E., … Saleem, M. (2017). An ecoregion-based approach to protecting half the terrestrial realm. *BioScience*, 67, 534–545.",
 "Hedges, L. V., Gurevitch, J., & Curtis, P. S. (1999). The meta-analysis of response ratios in experimental ecology. *Ecology*, 80, 1150–1156.",
 "Lajeunesse, M. J. (2011). On the meta-analysis of response ratios for studies with correlated and multi-group designs. *Ecology*, 92, 2049–2055.",
 "Nakagawa, S., Lagisz, M., O’Dea, R. E., Rutkowska, J., Yang, Y., Noble, D. W. A., & Senior, A. M. (2021). The orchard plot: Cultivating a forest plot for use in ecology, evolution, and beyond. *Research Synthesis Methods*, 12, 4–12.",
 "Nakagawa, S., Lagisz, M., Jennions, M. D., Koricheva, J., Noble, D. W. A., Parker, T. H., … O’Dea, R. E. (2022). Methods for testing publication bias in ecological and evolutionary meta-analyses. *Methods in Ecology and Evolution*, 13, 4–21.",
 "Nakagawa, S., & Santos, E. S. A. (2012). Methodological issues and advances in biological meta-analysis. *Evolutionary Ecology*, 26, 1253–1274.",
 "Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for cluster-robust variance estimation and hypothesis testing in fixed effects models. *Journal of Business & Economic Statistics*, 36, 672–683.",
 "R Core Team. (2024). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing.",
 "Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. *Journal of Statistical Software*, 36(3), 1–48.",
].forEach(r => C.push(new Paragraph({ children: runs(r), spacing: { after: 100, line: 360 }, indent: { left: 360, hanging: 360 } })));

C.push(new Paragraph({ children: [new PageBreak()] }));
C.push(H1("Figures"));
FIGS.forEach((f, i) => C.push(...figure(...f, i === 0)));

const doc = new Document({
  styles: { default: { document: { run: { font: FONT, size: 24 } } },
    paragraphStyles: [
      { id: "Heading1", name: "Heading 1", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 26, bold: true, font: FONT }, paragraph: { spacing: { before: 360, after: 120 }, outlineLevel: 0 } },
      { id: "Heading2", name: "Heading 2", basedOn: "Normal", next: "Normal", quickFormat: true,
        run: { size: 24, bold: true, italics: true, font: FONT }, paragraph: { spacing: { before: 240, after: 120 }, outlineLevel: 1 } }] },
  sections: [{ properties: { page: { margin: { top: 1440, bottom: 1440, left: 1440, right: 1440 } },
                             lineNumbers: { countBy: 1, restart: LineNumberRestartFormat.CONTINUOUS } },
               children: C }],
});
Packer.toBuffer(doc).then(b => { fs.writeFileSync(ROOT + "docs/Methods_Results.docx", b); console.log("ok"); });
