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
  ["Fig1_study_sites", 76, "**Figure 1** Distribution of the 79 studies (52 locations) included in the meta-analysis. Left: study sites, coloured by whether native, non-native (domestic or invasive) or both/mixed grazers were excluded; country borders are shown (Robinson projection). Right: number of studies on each continent, in decreasing order, with bars divided by the origin of the grazers excluded in each study."],
  ["Fig2_overall_origin", 115, "**Figure 2** Effects of grazer exclusion on invertebrate abundance, richness and diversity (a) overall and (b) by the origin of the excluded grazers (native vs. domestic). Small points are individual effect sizes (size proportional to precision, 1/SE); large points and bars are model estimates and 95% confidence intervals from multilevel meta-analytic models. The upper axis gives the equivalent percentage change in exclosures relative to grazed plots. Numbers give effect sizes (studies). Positive values indicate higher values where grazers were excluded."],
  ["Fig3_heterogeneity", 120, "**Figure 3** Variation in invertebrate responses to grazer exclusion. (a) Partitioning of the total variance in effect sizes into variation among studies and locations, among effect sizes within studies, and sampling error (multilevel *I*^2^). (b) Mean effect with its 95% confidence interval (black bar) and the 95% prediction interval (shaded band), the range within which the effect at a new site is expected to fall. (c) Percentage of the between-study and within-study heterogeneity explained by each moderator (pseudo-*R*^2^)."],
  ["Fig4_stratum_grazer_size", 135, "**Figure 4** Effects of grazer exclusion (a) by invertebrate stratum and (b) by the largest size class of grazer excluded (Small < 10 kg, Medium 10–100 kg, Large 100–1000 kg, Mega ≥ 1000 kg). Symbols as in Figure 2. Levels represented by fewer than three studies are not shown."],
  ["Fig5_time_since_exclusion", 65, "**Figure 5** Effects of grazer exclusion in relation to time since exclusion. Points are effect sizes (size proportional to precision); lines and shaded bands are meta-regression fits and 95% confidence intervals. The x-axis is ln(years + 1), labelled in years."],
  ["Fig6_trophic_level", 90, "**Figure 6** Effects of grazer exclusion on invertebrate trophic groups. Symbols as in Figure 2. Trophic groups represented by fewer than three studies within a response are not shown."],
  ["Fig7_plant_biomass", 70, "**Figure 7** Relationship between the effect of grazer exclusion on plant above-ground biomass and its effect on invertebrates, for studies that measured both: overall (abundance and richness pooled), abundance and richness. Each point is one study (study-level mean lnRR; size proportional to the precision of the invertebrate effect). Lines and shaded bands are least-squares fits and 95% confidence intervals. Labels give the Pearson correlation (*r*) with its *p* value and number of studies, and the slope and *p* value from the multilevel meta-regression fitted to all effect sizes. Dashed lines mark zero."],
  ["FigS3_time_by_stratum", 65, "**Figure S3** Effects of grazer exclusion on invertebrate abundance in relation to time since exclusion for (a) above-ground, (b) ground- and litter-dwelling and (c) below-ground invertebrates. Points are effect sizes (size proportional to precision); lines and shaded bands are stratum-specific meta-regression fits and 95% confidence intervals from a model with a Stratum × time interaction, drawn over the range of years observed in each stratum. Labels give each slope (per unit ln[years + 1]) with its *p* value and the number of effect sizes (studies). Slopes for richness and diversity are given in Table S1."],
  ["FigS1_biome", 95, "**Figure S1** Effects of grazer exclusion by biome (RESOLVE Ecoregions 2017). Symbols as in Figure 2. Biomes represented by fewer than three studies within a response are not shown."],
  ["FigS2_funnel", 65, "**Figure S2** Funnel plots of effect sizes against precision (1/SE, log scale). Vertical solid lines show the overall estimate, shaded regions its expected 95% range given each precision, dashed lines zero."],
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
C.push(P("*Biome* was assigned by intersecting each site with the RESOLVE Ecoregions 2017 map (Dinerstein et al., 2017). Sites falling just outside a polygon (coastal sites, coordinate rounding; six sites, all within 7 km) were assigned to the nearest ecoregion. Thirteen studies that did not report coordinates were located from their site descriptions; for two of these (one in Spain without a named site, one spanning 16 stands across Sweden) the assignment is approximate. *Continent* was assigned from the study country (Greenland was assigned to North America). To summarise the distribution of studies (Figure 1), each study was classed by the origin of the grazers it excluded as native, non-native (domestic livestock or invasive grazers) or mixed (both native and non-native grazers excluded, or a mixed assemblage)."));

C.push(H2("2.3 | Effect size"));
C.push(P("Effects were expressed as the natural log response ratio, lnRR = ln(*X̄*~E~ / *X̄*~G~), where *X̄*~E~ and *X̄*~G~ are the mean response in exclosures and grazed plots respectively (Hedges et al., 1999). Positive values indicate higher values where grazers were excluded. Standard errors were converted to standard deviations (SD = SE × √*n*) before computing lnRR and its sampling variance with the *escalc* function (measure ‘ROM’) of *metafor*. For presentation, estimates were back-transformed to percentage change, 100 × (e^lnRR^ − 1)."));

C.push(H2("2.4 | Statistical analyses"));
C.push(P("All models were multilevel random-effects meta-analyses fitted by restricted maximum likelihood with *rma.mv* in *metafor* v4.4 (Viechtbauer, 2010) in R (R Core Team, 2024). Random intercepts were included for location, study nested within location, and effect size nested within study. Where several comparisons within a study shared the same exclosure or grazed group, the resulting sampling covariance (the shared group’s contribution, SD^2^ / (*n* × *X̄*^2^)) was included in the variance–covariance matrix (Lajeunesse, 2011). Tests and confidence intervals used *t*-distributions with containment degrees of freedom. Because the number of locations was modest and effect sizes were strongly clustered, we also report cluster-robust (CR2) standard errors, confidence intervals and Wald tests, clustered by location, with small-sample corrections (Pustejovsky & Tipton, 2018; *clubSandwich*). We regard effects as robust only when they are supported by both approaches."));
C.push(P("Because responses to grazer exclusion are known to vary widely among sites, we quantified this variation explicitly. Cochran’s *Q* tests whether effect sizes vary more than expected from sampling error alone. The multilevel *I*^2^ expresses the proportion of the total variance in effect sizes attributable to true differences rather than sampling error, and partitions it among locations, studies within locations and effect sizes within studies (Nakagawa & Santos, 2012). Because most locations contributed a single study, the location and study components are poorly separated, so we interpret their sum (variation among studies and locations) alongside the within-study component. Because *I*^2^ is a relative measure, we also report 95% prediction intervals for the overall effects, which give the range within which the effect of grazer exclusion at a new site is expected to fall (Borenstein et al., 2017; IntHout et al., 2016). From the same distribution of true effects we estimated the proportion of sites at which exclusion is expected to have a negative effect. Finally, for each moderator we calculated the proportion of heterogeneity it explained as 1 − Σσ^2^~moderator~ / Σσ^2^~null~, where Σσ^2^ is the sum of the variance components of the model with and without the moderator fitted to the same data (pseudo-*R*^2^, truncated at zero; Raudenbush, 2009)."));
C.push(P("Each moderator was tested in a separate model within each response. For categorical moderators we report level-specific estimates from a model without an intercept, and an omnibus *F*-test of differences among levels from the equivalent model with an intercept. Levels represented by fewer than three studies were excluded from that model. Grazer size was additionally tested as a linear trend across ordered classes. Time since exclusion was analysed as a continuous moderator. To test whether the change in effects over time differed among strata, we fitted a model with stratum, time since exclusion and their interaction for each response, tested the interaction terms jointly with an omnibus *F*-test (model-based and cluster-robust), and estimated the slope for each stratum from the equivalent model with stratum-specific intercepts and slopes. Grazer origin (native vs. domestic) was tested first; because it did not explain variation in any response (see Results), all other moderators were analysed with comparisons pooled across origins."));
C.push(P("To test whether invertebrate responses track the response of the vegetation, we used the studies that also reported plant above-ground biomass in exclosures and grazed plots. Plant lnRR was calculated as for invertebrates and averaged within each study (inverse-variance weighted, assuming a correlation of 0.5 among a study’s effects), giving one plant response per study. One plant comparison lacking a sample size was excluded. Analyses were run for abundance and richness separately and for the two pooled (‘overall’). First, we calculated the Pearson correlation between the study-level plant lnRR and the study-level invertebrate lnRR (averaged in the same way), with Spearman’s rank correlation as a check against the influence of single studies. Second, the study-level plant lnRR was added as a continuous moderator to the multilevel model of the invertebrate effect sizes described above, which uses every effect size and weights by precision; in the overall model, abundance and richness had separate intercepts and a common slope."));
C.push(P("Robustness was assessed by (i) comparing exclosure experiments with natural grazed vs. ungrazed comparisons; (ii) refitting the overall models without comparisons in which a zero mean had been adjusted; (iii) leave-one-study-out refits of the overall models; and (iv) a multilevel Egger-type regression with the effect-size standard error as a moderator, whose intercept provides a small-study-adjusted estimate (Nakagawa et al., 2022), alongside funnel plots. Biome and continent were examined as additional exploratory moderators. Figures were produced with *ggplot2* and follow the orchard-plot format (Nakagawa et al., 2021). Code and data are available at [repository URL]."));

// ---------------------------------------------------------------- results
C.push(H1("3 | RESULTS"));
C.push(H2("3.1 | Dataset"));
C.push(P("The final dataset comprised 578 effect sizes from 79 studies at 52 locations in 22 countries, published between 1992 and 2026 (Figure 1). Of these, 417 effect sizes (68 studies, 44 locations) concerned abundance, 115 (35 studies, 31 locations) richness and 46 (17 studies, 16 locations) diversity. Most comparisons came from exclosure experiments (*k* = 533; natural grazed vs. ungrazed comparisons, *k* = 45). Exclusion had been in place for 0–54 years (median 4 years). Comparisons involved the exclusion of native grazers (*k* = 318), domestic grazers (*k* = 238), mixed assemblages (*k* = 19) or invasive grazers (*k* = 3)."));

C.push(H2("3.2 | Distribution of studies"));
C.push(P("Studies were unevenly distributed geographically (Figure 1). Europe contributed the most studies (32; 41%), followed by Asia (18), North America (17) and Africa (10); Oceania was represented by two studies in Australia, and there were no studies from South America. China (16 studies) and the USA (15) together contributed over a third of all studies, followed by the UK (9), Kenya and Switzerland (6 each); 11 of the 22 countries contributed a single study. Sites spanned latitudes from 36.8°S to 74.5°N, but 73 studies were in the Northern Hemisphere and 49 (62%) lay between 30° and 60°N. Eleven of the 13 RESOLVE biomes were represented, dominated by temperate broadleaf forest (20 studies), temperate conifer forest (16) and temperate grassland (14), with fewer studies in tropical savanna (8), Mediterranean (6), montane grassland (5), boreal forest (3), tundra (3), tropical moist forest (2), desert (1) and tropical conifer forest (1)."));
C.push(P("The type of grazer excluded differed among regions. Overall, 39 studies excluded native grazers, 34 non-native grazers (domestic livestock in all but one study) and six a mixture of both. Studies in North America (15 of 17) and Africa (7 of 10, the remainder mixed) mostly excluded native grazers, whereas studies in Asia mostly excluded livestock (15 of 18); European studies were split between native (14) and non-native grazers (16; Figure 1). Grazer origin and region are therefore partly confounded. Most studies excluded medium-sized (10–100 kg; 39 studies) or large grazers (100–1000 kg; 34), with few excluding megaherbivores (8) or only small grazers (5)."));
C.push(P("Above-ground invertebrates were studied most often (41 studies), followed by ground- and litter-dwelling (31) and below-ground invertebrates (15). Abundance was reported by 68 studies, richness by 35 and diversity by 17. Most studies used fenced exclosures (68), and 11 compared naturally ungrazed with grazed areas. Studies contributed a median of four effect sizes (range 1–56). Of the 69 studies reporting when exclusion began, the longest exclusion sampled had a median duration of 5 years (range 0–54); 44 (64%) sampled within the first 5 years and 22 (32%) after at least 10 years. The literature is recent: half of the studies were published in or after 2017."));

C.push(H2("3.3 | Overall effects of grazer exclusion and grazer origin"));
C.push(P("On average, excluding grazers increased invertebrate abundance by 14.8% (lnRR = 0.138, 95% CI: 0.011 to 0.265, *p* = .034; robust *p* = .025) and taxonomic richness by 24.1% (lnRR = 0.216, 95% CI: 0.038 to 0.395, *p* = .019; robust *p* = .019), but had no detectable effect on diversity (+10.3%, 95% CI: −15.9% to +44.8%, *p* = .45; Figure 2a; Table 1). Leave-one-study-out estimates ranged from 0.098 to 0.157 for abundance, 0.159 to 0.238 for richness and 0.008 to 0.150 for diversity, so no single study determined these averages."));
C.push(P("Responses did not differ between the exclusion of native and domestic grazers for abundance (native +15.8%, domestic +10.8%; *F*~1,394~ = 0.14, *p* = .71), richness (native +28.2%, domestic +21.9%; *F*~1,32~ = 0.17, *p* = .69) or diversity (native +4.8%, domestic +18.4%; *F*~1,15~ = 0.50, *p* = .49; Figure 2b; Table 1). Grazer origin was therefore not considered further, and all subsequent analyses pool comparisons across origins."));

C.push(H2("3.4 | Variation in responses among studies and sites"));
C.push(P("The average effects concealed very large variation. Effect sizes varied far more than expected from sampling error for all three responses (abundance: *Q*~416~ = 2851, richness: *Q*~114~ = 719, diversity: *Q*~45~ = 322; all *p* < .001), and true differences among effects accounted for 96.6%, 95.6% and 99.6% of the total variance, respectively (Figure 3a). Where this variation lay differed between responses. For abundance, it was split almost equally between differences among studies and locations (*I*^2^ = 53.9%) and differences among effect sizes within the same study (42.7%), that is, among taxa, strata and sampling years at the same site. For richness and diversity, by contrast, nearly all of the variation occurred among studies and locations (91.9% and 89.4%), and effects from the same study were consistent (within-study *I*^2^ = 3.8% and 10.2%)."));
C.push(P("As a consequence, the outcome of excluding grazers at any particular site is highly uncertain. The 95% prediction intervals, which give the range of effects expected at a new site, spanned from a 63% decrease to a 252% increase in abundance (lnRR −0.98 to 1.26), from a 52% decrease to a 223% increase in richness (−0.74 to 1.17) and from a 64% decrease to a 235% increase in diversity (−1.01 to 1.21; Figure 3b). Assuming normally distributed true effects, the effect of grazer exclusion is expected to be negative at 40% of sites for abundance, 32% for richness and 42% for diversity. Thus, although exclusion increases abundance and richness on average, it reduces them at a large minority of sites."));
C.push(P("None of the moderators examined accounted for an appreciable share of this variation (Figure 3c; Table 1). Grazer origin, stratum, grazer size, time since exclusion, trophic group and biome each explained between 0% and 3.2% of the heterogeneity in abundance and richness, and between 0% and 2.8% for diversity. The largest values were for time since exclusion (3.2%, abundance) and a linear trend in grazer size for diversity (13.4%; 16 studies, *p* = .086). The variation among sites therefore remained almost entirely unexplained by the study characteristics available."));

C.push(H2("3.5 | Stratum and grazer size"));
C.push(P("Responses did not differ significantly among strata for abundance (*F*~2,413~ = 1.05, *p* = .35), richness (*F*~2,112~ = 0.81, *p* = .45) or diversity (*F*~2,14~ = 0.50, *p* = .62; Figure 4a; Table 1). Nevertheless, the increase in abundance was concentrated in above-ground invertebrates (+22.6%, 95% CI: +5.1% to +43.0%, *p* = .010; robust *p* = .024), whereas ground- and litter-dwelling (+8.6%, *p* = .34) and below-ground invertebrates (+9.8%, *p* = .48) showed no significant change. Richness increased in above-ground (+26.6%, 95% CI: +2.3% to +56.7%, *p* = .031; robust *p* = .051) and ground- and litter-dwelling assemblages (+24.9%, 95% CI: +2.3% to +52.4%, *p* = .029; robust *p* = .032), but not below ground (+15.0%, *p* = .22)."));
C.push(P("Abundance responses differed among the size classes of excluded grazers in the model-based test (*F*~3,369~ = 9.14, *p* < .001), but not in the cluster-robust test (robust *p* = .38; Figure 4b), and grazer size explained none of the heterogeneity in abundance. The difference was driven by a large increase following the exclusion of small grazers (+276%, 95% CI: +124% to +533%), estimated from only eight effect sizes from three studies at two locations. Excluding medium-sized (−3.2%, *p* = .76) or mega-herbivores (−7.8%, *p* = .68) had no detectable effect on abundance, whereas excluding large grazers increased abundance marginally (+22.5%, 95% CI: +0.3% to +49.6%, *p* = .047; robust *p* = .12), with responses varying widely among locations. There was no linear trend in abundance responses across size classes (slope = −0.047 per class, *p* = .54). Excluding large grazers increased richness (+36.3%, 95% CI: +9.7% to +69.4%, *p* = .006; robust *p* = .043), but richness responses did not differ significantly between medium and large grazers (*F*~1,92~ = 2.24, *p* = .14). Diversity did not vary with grazer size (*F*~1,34~ = 1.51, *p* = .23; linear trend *p* = .086)."));

C.push(H2("3.6 | Time since exclusion"));
C.push(P("Abundance responses tended to decline with time since exclusion (slope = −0.108 per unit ln[years + 1], 95% CI: −0.226 to 0.011, *p* = .074; robust *p* = .39), from an estimated +41% at the start of exclusion to close to zero after several decades (Figure 5), although time since exclusion explained only 3.2% of the heterogeneity in abundance. Time since exclusion was unrelated to richness (slope = 0.075, *p* = .38) or diversity responses (slope = −0.012, *p* = .93)."));
C.push(P("The change in effects over time did not differ among strata for abundance (Stratum × time: *F*~2,394~ = 1.73, *p* = .18; robust *p* = .48), richness (*F*~2,97~ = 0.21, *p* = .81; robust *p* = .78) or diversity (*F*~2,9~ = 0.76, *p* = .49; robust *p* = .50; Table S1). For abundance (Figure S3), the decline over time was confined to ground- and litter-dwelling invertebrates (slope = −0.178, 95% CI: −0.344 to −0.011, *p* = .037; robust *p* = .13) and, less precisely, below-ground invertebrates (−0.179, 95% CI: −0.486 to 0.128, *p* = .25), whereas the response of above-ground invertebrates did not change with time (−0.013, 95% CI: −0.163 to 0.138, *p* = .87). The difference in slope between ground-dwelling and above-ground invertebrates was not significant (−0.165, *p* = .077; robust *p* = .24). Adding the interaction explained little additional heterogeneity in abundance (4.7% for the full model vs. 4.3% for stratum and time without the interaction). No stratum showed a significant change over time in richness (slopes 0.045 to 0.127, all *p* ≥ .37) or diversity (all *p* ≥ .21), although diversity was represented by only four to six studies per stratum."));

C.push(H2("3.7 | Trophic groups"));
C.push(P("Trophic groups did not differ significantly in their abundance responses to grazer exclusion (*F*~7,320~ = 1.27, *p* = .27; Figure 6; Table 2), and trophic group explained none of the heterogeneity in any response. Point estimates were positive for most groups and largest for parasitoids (+51.0%, 95% CI: +2.1% to +123.1%, *p* = .039; four studies), bacterivorous nematodes (+37.6%, *p* = .097) and detritivores (+35.8%, 95% CI: +4.3% to +76.9%, *p* = .023), intermediate for predators (+18.5%, *p* = .13) and herbivores (+12.8%, *p* = .26), and negative only for pollinators (−13.0%, *p* = .53). None of the group-level effects was significant in cluster-robust tests (all robust *p* ≥ .17). Richness (*F*~2,66~ = 0.42, *p* = .66) and diversity (*F*~1,13~ = 0.02, *p* = .90) responses likewise did not differ among trophic groups."));

C.push(H2("3.8 | Biome and continent"));
C.push(P("Responses did not differ among biomes for abundance (*F*~6,33~ = 1.21, *p* = .33), richness (*F*~4,20~ = 0.36, *p* = .83) or diversity (*F*~3,8~ = 0.93, *p* = .47; Figure S1), and biome explained none of the heterogeneity. The only biome-level estimate that differed from zero was an increase in abundance in temperate conifer forests (+59.9%, 95% CI: +10.4% to +131.5%, *p* = .015), which came from 15 studies at seven locations and was not supported by the cluster-robust test (robust *p* = .19). Likewise, responses did not differ among continents for abundance (*F*~3,38~ = 0.40, *p* = .75), richness (*F*~3,26~ = 0.27, *p* = .85) or diversity (*F*~2,12~ = 0.01, *p* = .99)."));

C.push(H2("3.9 | Invertebrate responses and plant biomass"));
C.push(P("Sixteen studies reported plant above-ground biomass, 15 of which also reported invertebrate abundance or richness. In these studies, excluding grazers increased plant biomass by 47.5% on average (95% CI: +10.6% to +96.6%, *p* = .012), with responses ranging from an 11% decrease to a more than sevenfold increase. Invertebrate responses were not correlated with the plant-biomass response, either overall (*r* = −0.04, 95% CI: −0.54 to 0.48, *p* = .88, *n* = 15 studies), for abundance (*r* = −0.01, 95% CI: −0.54 to 0.52, *p* = .97, *n* = 14) or for richness (*r* = 0.21, 95% CI: −0.58 to 0.80, *p* = .61, *n* = 8; Figure 7; Table 3). Rank correlations gave the same result (Spearman’s ρ = −0.19, −0.13 and 0.33; all *p* ≥ .43). The meta-regressions, which used all effect sizes, likewise found no relationship (overall slope = −0.050, 95% CI: −0.423 to 0.322, *p* = .77; abundance −0.066, −0.571 to 0.440, *p* = .78; richness 0.053, −0.287 to 0.393, *p* = .72; all robust *p* ≥ .47), and the plant response explained none of the heterogeneity in invertebrate responses. Thus, in this subset of studies, sites where grazer exclusion increased plant biomass most did not show larger increases in invertebrate abundance or richness, although the small number of studies, particularly for richness, means that only strong relationships could have been detected."));

C.push(H2("3.10 | Sensitivity analyses and publication bias"));
C.push(P("Effects on abundance were positive in exclosure experiments (+17.8%, 95% CI: +2.9% to +34.9%, *p* = .019) but not in natural grazed vs. ungrazed comparisons (−5.2%, *p* = .77), although the two designs did not differ significantly (*F*~1,42~ = 1.26, *p* = .27); design did not affect richness (*p* = .39) or diversity (*p* = .12). Excluding comparisons with zero-adjusted means did not alter the overall abundance estimate (+14.0%, 95% CI: +0.9% to +28.9%). The multilevel Egger test revealed strong small-study asymmetry for abundance (slope on SE = 0.757, 95% CI: 0.449 to 1.066, *p* < .001; robust *p* = .003): less precise studies reported larger positive effects (Figure S2). The bias-adjusted intercept for abundance was close to zero (−8.3%, 95% CI: −21.4% to +7.1%, *p* = .27), indicating that the overall increase in abundance should be interpreted with caution. There was no evidence of small-study effects for richness (*p* = .12) or diversity (*p* = .47)."));

// ---------------------------------------------------------------- tables
C.push(new Paragraph({ children: [new PageBreak()] }));
C.push(new Paragraph({ children: runs("TABLE 1 Overall effects of grazer exclusion, heterogeneity and moderator tests for each response. Estimates are percentage change (95% CI) in exclosures relative to grazed plots. Moderator tests are model-based *F*-tests with containment degrees of freedom (cluster-robust *p* in brackets); *R*^2^ is the percentage of heterogeneity explained by the moderator."), spacing: { after: 120 } }));
C.push(...table(
  ["", "Abundance", "Richness", "Diversity"],
  [["Overall effect, % (95% CI)", "+14.8 (+1.1, +30.3)", "+24.1 (+3.9, +48.4)", "+10.3 (−15.9, +44.8)"],
   ["*p* (robust *p*)", ".034 (.025)", ".019 (.019)", ".45 (.45)"],
   ["95% prediction interval, %", "−62.6 to +252.3", "−52.3 to +223.1", "−63.7 to +235.4"],
   ["Sites with negative effect (%)", "40", "32", "42"],
   ["*k* / studies / locations", "417 / 68 / 44", "115 / 35 / 31", "46 / 17 / 16"],
   ["*Q* (df)", "2851 (416)***", "719 (114)***", "322 (45)***"],
   ["*I*^2^ total (%)", "96.6", "95.6", "99.6"],
   ["*I*^2^ among studies/locations (%)", "53.9", "91.9", "89.4"],
   ["*I*^2^ within studies (%)", "42.7", "3.8", "10.2"],
   ["Grazer origin", "*F*~1,394~ = 0.14, *p* = .71 (.69); *R*^2^ = 0", "*F*~1,32~ = 0.17, *p* = .69 (.78); *R*^2^ = 0", "*F*~1,15~ = 0.50, *p* = .49 (.41); *R*^2^ = 0"],
   ["Stratum", "*F*~2,413~ = 1.05, *p* = .35 (.35); *R*^2^ = 1.7", "*F*~2,112~ = 0.81, *p* = .45 (.37); *R*^2^ = 0", "*F*~2,14~ = 0.50, *p* = .62 (.34); *R*^2^ = 0"],
   ["Grazer size (largest class)", "*F*~3,369~ = 9.14, *p* < .001 (.38); *R*^2^ = 0", "*F*~1,92~ = 2.24, *p* = .14 (.31); *R*^2^ = 1.8", "*F*~1,34~ = 1.51, *p* = .23 (.48); *R*^2^ = 2.8"],
   ["Grazer size (linear trend)", "*p* = .54 (.71); *R*^2^ = 0", "*p* = .98 (.99); *R*^2^ = 0", "*p* = .086 (.16); *R*^2^ = 13.4"],
   ["Time since exclusion", "*p* = .074 (.39); *R*^2^ = 3.2", "*p* = .38 (.24); *R*^2^ = 0", "*p* = .93 (.85); *R*^2^ = 0"],
   ["Trophic group", "*F*~7,320~ = 1.27, *p* = .27 (–); *R*^2^ = 0", "*F*~2,66~ = 0.42, *p* = .66 (.79); *R*^2^ = 0", "*F*~1,13~ = 0.02, *p* = .90 (.83); *R*^2^ = 0"],
   ["Biome", "*F*~6,33~ = 1.21, *p* = .33 (.93); *R*^2^ = 0", "*F*~4,20~ = 0.36, *p* = .83 (.16); *R*^2^ = 0", "*F*~3,8~ = 0.93, *p* = .47 (.82); *R*^2^ = 0"],
   ["Continent", "*F*~3,38~ = 0.40, *p* = .75 (.90)", "*F*~3,26~ = 0.27, *p* = .85 (.77)", "*F*~2,12~ = 0.01, *p* = .99 (.99)"],
   ["Study design", "*F*~1,42~ = 1.26, *p* = .27 (.28)", "*F*~1,33~ = 0.77, *p* = .39 (.42)", "*F*~1,15~ = 2.68, *p* = .12 (.18)"],
   ["Egger test (SE slope)", "*p* < .001 (.003)", "*p* = .12 (.086)", "*p* = .47 (.70)"]],
  [2600, 2280, 2280, 2200],
  "*Note*: ****p* < .001. The robust omnibus test for trophic group (abundance) could not be computed because the number of levels was large relative to the number of locations."));

C.push(new Paragraph({ children: runs("TABLE 2 Effects of grazer exclusion on the abundance of invertebrate trophic groups. Values are percentage change in exclosures relative to grazed plots (95% CI); *p* values are model-based (cluster-robust in brackets)."), spacing: { before: 240, after: 120 } }));
C.push(...table(
  ["Trophic group", "Change, % (95% CI)", "*p* (robust *p*)", "*k* (studies)"],
  [["Herbivore", "+12.8 (−8.6, +39.1)", ".26 (.35)", "119 (35)"],
   ["Pollinator", "−13.0 (−43.6, +34.2)", ".53 (.61)", "19 (6)"],
   ["Omnivore", "+3.9 (−25.5, +44.8)", ".82 (.89)", "26 (5)"],
   ["Predator", "+18.5 (−5.0, +47.8)", ".13 (.19)", "77 (24)"],
   ["Parasitoid", "+51.0 (+2.1, +123.1)", ".039 (.24)", "16 (4)"],
   ["Detritivore", "+35.8 (+4.3, +76.9)", ".023 (.17)", "47 (14)"],
   ["Bacterivore", "+37.6 (−5.6, +100.5)", ".097 (.19)", "12 (4)"],
   ["Fungivore", "+9.2 (−23.2, +55.2)", ".62 (.65)", "12 (4)"]],
  [2400, 2600, 2200, 2160],
  "*Note*: Scavengers and algivores (one study each) were not estimated."));

C.push(new Paragraph({ children: runs("TABLE 3 Relationship between invertebrate and plant above-ground biomass responses to grazer exclusion. Correlations use one value per study; meta-regression slopes are from multilevel models fitted to all invertebrate effect sizes (lnRR change in invertebrates per unit lnRR change in plant biomass), with cluster-robust *p* in brackets."), spacing: { before: 240, after: 120 } }));
C.push(...table(
  ["", "Overall (abundance + richness)", "Abundance", "Richness"],
  [["Studies (effect sizes)", "15 (188)", "14 (143)", "8 (45)"],
   ["Pearson *r* (95% CI)", "−0.04 (−0.54, 0.48)", "−0.01 (−0.54, 0.52)", "0.21 (−0.58, 0.80)"],
   ["*p*", ".88", ".97", ".61"],
   ["Spearman ρ (*p*)", "−0.19 (.51)", "−0.13 (.66)", "0.33 (.43)"],
   ["Meta-regression slope (95% CI)", "−0.050 (−0.423, 0.322)", "−0.066 (−0.571, 0.440)", "0.053 (−0.287, 0.393)"],
   ["*p* (robust *p*)", ".77 (.73)", ".78 (.75)", ".72 (.47)"],
   ["*R*^2^ (%)", "0", "0", "0"]],
  [2600, 2400, 2200, 2160],
  "*Note*: The overall meta-regression includes separate intercepts for abundance and richness. Diversity (four studies) was not analysed."));

C.push(new Paragraph({ children: runs("TABLE S1 Change in the effect of grazer exclusion with time since exclusion (slope per unit ln[years + 1]) for each invertebrate stratum, from multilevel models including a Stratum × time interaction. *p* values are model-based (cluster-robust in brackets)."), spacing: { before: 240, after: 120 } }));
C.push(...table(
  ["", "Above-ground", "Ground & litter-dwelling", "Below-ground", "Stratum × time"],
  [["Abundance", "−0.013 (−0.163, 0.138); .87 (.95)", "−0.178 (−0.344, −0.011); .037 (.13)", "−0.179 (−0.486, 0.128); .25 (.10)", "*F*~2,394~ = 1.73, *p* = .18 (.48)"],
   ["Richness", "0.127 (−0.152, 0.405); .37 (.28)", "0.051 (−0.185, 0.286); .67 (.68)", "0.045 (−0.194, 0.284); .71 (.39)", "*F*~2,97~ = 0.21, *p* = .81 (.78)"],
   ["Diversity", "−0.441 (−1.150, 0.268); .22 (.43)", "0.049 (−0.342, 0.440); .80 (.59)", "−0.111 (−1.026, 0.804); .79 (.44)", "*F*~2,9~ = 0.76, *p* = .49 (.50)"]],
  [1500, 2050, 2050, 2050, 1710],
  "*Note*: Values are slope (95% CI); *p* (robust *p*). Sample sizes (effect sizes / studies): abundance 184/33, 141/21, 75/13; richness 28/13, 45/11, 30/6; diversity 15/5, 6/4, 19/6 (above-ground, ground & litter-dwelling, below-ground)."));

C.push(H1("References"));
[
 "Borenstein, M., Higgins, J. P. T., Hedges, L. V., & Rothstein, H. R. (2017). Basics of meta-analysis: *I*^2^ is not an absolute measure of heterogeneity. *Research Synthesis Methods*, 8, 5–18.",
 "Dinerstein, E., Olson, D., Joshi, A., Vynne, C., Burgess, N. D., Wikramanayake, E., … Saleem, M. (2017). An ecoregion-based approach to protecting half the terrestrial realm. *BioScience*, 67, 534–545.",
 "Hedges, L. V., Gurevitch, J., & Curtis, P. S. (1999). The meta-analysis of response ratios in experimental ecology. *Ecology*, 80, 1150–1156.",
 "IntHout, J., Ioannidis, J. P. A., Rovers, M. M., & Goeman, J. J. (2016). Plea for routinely presenting prediction intervals in meta-analysis. *BMJ Open*, 6, e010247.",
 "Lajeunesse, M. J. (2011). On the meta-analysis of response ratios for studies with correlated and multi-group designs. *Ecology*, 92, 2049–2055.",
 "Nakagawa, S., Lagisz, M., O’Dea, R. E., Rutkowska, J., Yang, Y., Noble, D. W. A., & Senior, A. M. (2021). The orchard plot: Cultivating a forest plot for use in ecology, evolution, and beyond. *Research Synthesis Methods*, 12, 4–12.",
 "Nakagawa, S., Lagisz, M., Jennions, M. D., Koricheva, J., Noble, D. W. A., Parker, T. H., … O’Dea, R. E. (2022). Methods for testing publication bias in ecological and evolutionary meta-analyses. *Methods in Ecology and Evolution*, 13, 4–21.",
 "Nakagawa, S., & Santos, E. S. A. (2012). Methodological issues and advances in biological meta-analysis. *Evolutionary Ecology*, 26, 1253–1274.",
 "Pustejovsky, J. E., & Tipton, E. (2018). Small-sample methods for cluster-robust variance estimation and hypothesis testing in fixed effects models. *Journal of Business & Economic Statistics*, 36, 672–683.",
 "Raudenbush, S. W. (2009). Analyzing effect sizes: Random-effects models. In H. Cooper, L. V. Hedges, & J. C. Valentine (Eds.), *The handbook of research synthesis and meta-analysis* (2nd ed., pp. 295–315). Russell Sage Foundation.",
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
