library(ggplot2)
library(grid)

## set theme
theme_all <- theme(panel.background = element_rect(fill = NA),
                   plot.title = element_text(color = "black", face = "bold", hjust = 0.5, size = 10),
                   plot.subtitle = element_text(color = "black",hjust = 0.5,size= 8),
                   axis.text = element_text(color = "black", face = "bold", size = 8),
                   axis.title = element_text(color = "black", face = "bold", size = 8),
                   legend.text = element_text(color = "black", face = "bold", size = 8),
                   legend.title = element_text(color = "black", face = "bold", size = 8),
                   strip.text = element_text(color = "black", face = "bold", size = 10),
                   strip.background = element_blank())

## set color
col_facility = c("#252525", "#1b9e77")
names(col_facility) = c("Vivarium", "Farm")
col_treatment = c("#Ad61c6", "darkgreen")
names(col_treatment) = c("Chlorinated", "Diwater")
col_sex = c("#F1726D", "#2b8cbe")
names(col_sex) <- c("Female", "Male")
col_age = c("#F9966D", "#Da87e7", "navy")
names(col_age) = c("Week 3","Week 7","Week 12")

## set label
labs_sex <- c("Female", "Male")
names(labs_sex) <- c("F", "M")
labs_facility <- c("Vivarium", "Farm")
names(labs_facility) <- c("Vivarium", "Farm")
labs_treatment <- c("Chlorinated", "Diwater")
names(labs_treatment) <- c("Chlorinated", "Diwater")

## Panel save figures
save_fixed_panels <- function(
    plots,
    filename,
    ncol = length(plots),
    panel_width = 2,
    panel_height = 2,
    unit = "in",
    dpi = 300,
    tags = LETTERS,
    shared_legend = TRUE,
    legend_position = "right",
    legend_gap = 0.08,
    tag_x = 0.01,
    tag_y = 0.99,
    tag_size = 12
){
  n <- length(plots)
  nrow <- ceiling(n / ncol)
  # Helper: extract legend from a plot
  get_legend <- function(p) {
    g <- ggplotGrob(p + theme(legend.position = legend_position))
    # ggplot2 may contain several guide-box slots
    idx <- grep("^guide-box", g$layout$name)
    if (length(idx) == 0){return(NULL)}
    legends <- g$grobs[idx]
    # Keep only non-empty guide boxes
    keep <- vapply(
      legends,
      function(x) !inherits(x, "zeroGrob"),
      logical(1)
    )
    if (!any(keep)){return(NULL)}
    # Use the first non-empty legend
    legends[[which(keep)[1]]]
  }
  # Shared legend:
  #   TRUE  -> extract one legend and remove legends from plots
  #   FALSE -> keep each plot's own legend
  if (shared_legend) {
    legend_grob <- get_legend(plots[[1]])
    plots_draw <- lapply(plots,function(p){
      p + theme(legend.position = "none")
    })
  } else {
    legend_grob <- NULL
    plots_draw <- lapply(plots,function(p) {
      p + theme(legend.position = legend_position)
    })
  }
  # Convert ggplots to gtables
  gs <- lapply(plots_draw,ggplotGrob)
  # Force every panel to the same physical size
  gs <- lapply(gs, function(g) {
    panel_id <- grep("^panel", g$layout$name)
    panel_cols <- unique(g$layout$l[panel_id])
    panel_rows <- unique(g$layout$t[panel_id])
    # Set panel width
    g$widths[panel_cols] <- unit(panel_width, units = unit)
    # Set panel height
    g$heights[panel_rows] <- unit(panel_height, units = unit)
    g
  })
  # Measure the full size of each plot
  # including axis text, axis titles,
  # strip labels, titles, margins, etc.
  widths <- vapply(gs,function(g){
    convertWidth(
      sum(g$widths),unitTo = unit, valueOnly = TRUE
    )},
    numeric(1)
  )
  heights <- vapply(gs, function(g){
    convertHeight(
      sum(g$heights), unitTo = unit, valueOnly = TRUE
    )},
    numeric(1)
  )
  # Column widths:largest complete plot width
  col_widths <- sapply(seq_len(ncol), function(j){
    idx <- seq(from = j,to = n,by = ncol)
    max(widths[idx])
  })
  # Row heights:largest complete plot height
  row_heights <- sapply(seq_len(nrow), function(i){
    idx_start <- (i - 1) * ncol + 1
    idx_end <- min(i * ncol, n)
    idx <- idx_start:idx_end
    max(heights[idx])
  })
  
  # Main plot-region dimensions
  plot_width <- sum(col_widths)
  plot_height <- sum(row_heights)
  
  # Measure shared legend
  if (
    shared_legend &&
    !is.null(legend_grob)
  ) {
    legend_width <- convertWidth(
      grobWidth(legend_grob), unitTo = unit,valueOnly = TRUE
    )
    legend_height <- convertHeight(
      grobHeight(legend_grob), unitTo = unit, valueOnly = TRUE
    )
  } else {
    legend_width <- 0
    legend_height <- 0
  }
  # Calculate final output dimensions
  if (shared_legend &&
      !is.null(legend_grob) &&
      legend_position %in% c("right","left")){
    # Legend placed beside plots
    total_width <-plot_width + legend_gap +legend_width
    total_height <- max(plot_height,legend_height)
  } else if (shared_legend &&
             !is.null(legend_grob) &&
             legend_position %in% c("top","bottom")){
    # Legend placed above/below plots
    total_width <- max(plot_width,legend_width)
    total_height <- plot_height + legend_gap + legend_height
  } else {
    # No shared legend
    total_width <- plot_width
    total_height <- plot_height
  }
  # Open output device according
  ext <- tolower(tools::file_ext(filename))
  if (ext == "pdf") {
    pdf(filename, width = total_width, height = total_height, useDingbats = FALSE)
    
  } else if (ext %in% c("tif", "tiff")) {
    tiff(filename, width = total_width, height = total_height, 
         units = unit, res = dpi,compression = "lzw")
  } else if (ext == "png") {
    png(filename, width = total_width, height = total_height,
        units = "in", res = dpi)
  } else {stop("Use pdf, png, tif, or tiff.")}
  
  grid.newpage()
  
  # shared legend on right or left
  if (shared_legend &&
      !is.null(legend_grob) &&
      legend_position %in% c("right","left")) {
    if (legend_position == "right") {
      outer <- grid.layout(nrow = 1, ncol = 3,
                           widths = unit(c(plot_width,legend_gap,legend_width),units = unit)
      )
      plot_col <- 1
      legend_col <- 3
    } else {
      outer <- grid.layout(nrow = 1, ncol = 3,
                           widths = unit(c(legend_width, legend_gap,plot_width),units = unit)
      )
      legend_col <- 1
      plot_col <- 3
    }
    pushViewport(
      viewport(
        layout = outer
      )
    )
    # Main plot region
    pushViewport(
      viewport(
        layout.pos.row = 1,
        layout.pos.col = plot_col
      )
    )
    lay <- grid.layout(nrow = nrow, ncol = ncol,
                       widths = unit(col_widths, units = unit),
                       heights = unit(row_heights, units = unit)
    )
    pushViewport(
      viewport(
        layout = lay
      )
    )
    # Draw each plot
    for (i in seq_along(gs)) {
      r <- ceiling(i / ncol)
      c <- ((i - 1) %% ncol) + 1
      pushViewport(
        viewport(
          layout.pos.row = r,
          layout.pos.col = c
        )
      )
      # Draw complete plot
      grid.draw(gs[[i]])
      # Draw panel tag inside plot cell
      if (!is.null(tags) &&
          i <= length(tags)){
        grid.text(
          tags[i],
          x = unit(tag_x, "npc"),
          y = unit(tag_y, "npc"),
          just = c("left","top"),
          gp = gpar(
            fontsize = tag_size,
            fontface = "bold"
          )
        )
      }
      popViewport()
    }
    popViewport()
    popViewport()
    # Draw shared legend
    pushViewport(
      viewport(
        layout.pos.row = 1,
        layout.pos.col = legend_col
      )
    )
    grid.draw(legend_grob)
    popViewport()
    popViewport()
    # no shared legend
    # or unsupported legend orientation
  } else {
    lay <- grid.layout(
      nrow = nrow,
      ncol = ncol,
      widths = unit(col_widths, units = unit),
      heights = unit(row_heights, units = unit)
    )
    pushViewport(
      viewport(
        layout = lay
      )
    )
    # Draw plots
    for (i in seq_along(gs)) {
      r <- ceiling(i / ncol)
      c <- ((i - 1) %% ncol) + 1
      pushViewport(
        viewport(
          layout.pos.row = r,
          layout.pos.col = c
        )
      )
      
      grid.draw(gs[[i]])
      # Draw panel tags inside plot cells
      if (!is.null(tags) &&
          i <= length(tags)) {
        grid.text(
          tags[i],
          x = unit(tag_x, "npc"),
          y = unit(tag_y, "npc"),
          just = c("left","top"),
          gp = gpar(
            fontsize = tag_size,
            fontface = "bold"
          )
        )
      }
      popViewport()
    }
    popViewport()
  }
  dev.off()
  
  invisible(
    c(
      width = total_width,
      height = total_height
    )
  )
}

## manual permutation 
family_permutations = function(meta, nperm = 999) {
  groups = split(seq_len(nrow(meta)), as.character(meta$FamilyID))
  slot = paste(meta$Sex, meta$Week, sep = "|")
  stopifnot(all(vapply(groups, function(ii) !anyDuplicated(slot[ii]), logical(1))))
  pattern = vapply(groups, function(ii) paste(sort(slot[ii]), collapse = ";"), character(1))
  sets = split(names(groups), pattern)
  ans = matrix(rep(seq_len(nrow(meta)), nperm), nrow = nperm, byrow = T)
  for (b in seq_len(nperm)) for (families in sets) {
    source = sample(families, length(families))
    for (j in seq_along(families)) {
      target = groups[[families[j]]]
      src = groups[[source[j]]]
      ans[b, target] = src[match(slot[target], slot[src])]
    }
  }
  ans
}

sex_permutations = function(meta, nperm = 999) {
  groups = split(seq_len(nrow(meta)), as.character(meta$FamilyID))
  ans = matrix(rep(seq_len(nrow(meta)), nperm), nrow = nperm, byrow = T)
  for (b in seq_len(nperm)) for (ii in groups) {
    female = ii[meta$Sex[ii] == "Female"]
    male = ii[meta$Sex[ii] == "Male"]
    if (length(female) == length(male) && length(female) > 0 &&
        setequal(meta$Week[female], meta$Week[male]) && sample(c(F,T), 1)) {
      ans[b, female] = male[match(meta$Week[female], meta$Week[male])]
      ans[b, male] = female[match(meta$Week[male], meta$Week[female])]
    }
  }
  ans
}

time_permutations = function(meta, nperm = 999) {
  groups = split(seq_len(nrow(meta)), as.character(meta$FamilyID))
  ans = matrix(rep(seq_len(nrow(meta)), nperm), nrow = nperm, byrow = T)
  for (b in seq_len(nperm)) for (ii in groups) {
    subjects = split(ii, meta$SubjectID[ii], drop = T)
    patterns = vapply(subjects, function(jj) paste(sort(as.character(meta$Week[jj])), collapse=";"), character(1))
    # Incomplete patterns cannot be jointly permuted; keep that family fixed.
    if (length(unique(patterns)) != 1) next
    weeks = as.character(meta$Week[subjects[[1]]])
    shuffled = sample(weeks, length(weeks))
    for (jj in subjects) ans[b,jj] = jj[match(shuffled[match(as.character(meta$Week[jj]), weeks)], meta$Week[jj])]
  }
  ans
}
## PERMANOVA terms and interaction 
test_terms = function(distance, meta, rhs, perm, wanted, analysis, metric, scheme) {
  model = dbrda(as.formula(paste("distance ~", rhs)), data = meta)
  a = as.data.frame(anova(model, permutations = perm, by = "margin", scope = wanted, model = "reduced")) %>%
    rownames_to_column("term")
  variance = intersect(c("Variance", "SumOfSqs"), names(a))[1]
  a = a |>filter(term %in% wanted) %>%
    transmute(term, Df, sum_of_squares = .data[[variance]],
              R2_total = .data[[variance]] / model$tot.chi,
              F = F, p = `Pr(>F)`, analysis = analysis, metric = metric,
              permutation_scheme = scheme, n_samples = nrow(meta),
              n_mice = n_distinct(meta$SubjectID), n_families = n_distinct(meta$FamilyID),
              permutations = nrow(perm), formula = rhs)
  a
}

library(Maaslin2)
### function wrap for all taxa levels
#in put metadata, sample id column name, phyloseq object
do_maaslin2 = function(meta, sample_id, phylo_rf, fixed, random, ref, output, 
                       min_prevalence = 0.1, min_abundance = 0.0001, max_significance = 0.05,
                       normalization = "TSS", transform = "LOG"){
  ft = otu_table(phylo_rf@otu_table) |>as.matrix() |>as.data.frame() |>rownames_to_column(var = "id")
  wft = ft |>select(id,  meta |>pull(sample_id)) |>column_to_rownames(var = "id") |>t()
  wmeta = meta |>column_to_rownames(var = sample_id)
  taxrank = "ASV"
  fit = Maaslin2(
    input_data = wft, 
    input_metadata = wmeta, 
    normalization = normalization,
    transform = transform,
    output = paste0(output,"_",taxrank),
    plot_heatmap = F,
    plot_scatter = F,
    fixed_effects = fixed,
    reference = ref,
    random_effects = random,
    min_prevalence = min_prevalence,
    min_abundance = min_abundance,
    max_significance = max_significance
  )
  for (taxrank in c("Species","Genus","Family")){
    phylo_glom = tax_glom(phylo_rf, taxrank = taxrank, NArm = F)
    ft_glom = otu_table(phylo_glom@otu_table) |>as.matrix() |>as.data.frame() |>rownames_to_column(var = "id")
    wft = ft_glom |>select(id, meta |>pull(sample_id)) |>column_to_rownames(var = "id") |>t()
    fit = Maaslin2(
      input_data = wft, 
      input_metadata = wmeta, 
      normalization = normalization,
      transform = transform,
      output = paste0(output,"_",taxrank),
      plot_heatmap = F,
      plot_scatter = F,
      fixed_effects = fixed,
      reference = ref,
      random_effects = random,
      min_prevalence = min_prevalence,
      min_abundance = min_abundance,
      max_significance = max_significance
    )
  }
}

summarise_maaslin2 = function(path, group, taxa){
  res = matrix(data = NA, nrow = 1, ncol = 25)
  res = as.data.frame(res)
  names(res) =  c("feature","metadata","value","coef","stderr","N","N.not.0","pval","qval","Group",
                  "id","Kingdom","Phylum","Class","Order","Family","Genus","Species","ASV", 
                  "Class_l", "Order_l", "Family_l", "Genus_l", "Species_l","level")
  for (taxrank in c("ASV", "Species", "Genus", "Family")){
    a = read.table(file = paste0(path ,"_", taxrank, "/significant_results.tsv"),
                   header = T, sep = "\t", stringsAsFactors = F)
    if (nrow(a) == 0) {next}
    a = a |>mutate(Group = group, id = str_remove(feature,"^X"),feature = str_remove(feature,"^X")) |>left_join(taxa, by = "id") |>mutate(level = taxrank)
    res = rbind(res,a)
  }
  res = res |>filter(!is.na(feature))
  res
}

plotting_volcano <- function(dds, group1, group2) {
  p_cutoff = 0.05
  fc_cutoff = 1
  
  res <- results(dds, contrast=c("age_fac_sex", group1, group2))
  res_df = as.data.frame(res) |>mutate(logPvalue = -log10(pvalue))
  
  #Volcano
  up = res_df |> dplyr::filter(padj < p_cutoff, log2FoldChange > fc_cutoff) |>nrow()
  down = res_df |> dplyr::filter(padj < p_cutoff, log2FoldChange < -fc_cutoff) |>nrow()
  g <- ggplot(res_df |>dplyr::filter(padj >= p_cutoff), aes(x = log2FoldChange, y = logPvalue)) + 
    geom_point(size = 1, color = "gray") + 
    geom_point(size = 1, data = res_df |>dplyr::filter(padj < p_cutoff), 
               aes(x = log2FoldChange, y = logPvalue),
               color = "blue") + 
    geom_point(size = 1, data = res_df |>dplyr::filter(padj < p_cutoff, abs(log2FoldChange) > fc_cutoff),
               aes(x = log2FoldChange, y = logPvalue),
               color = "red") + 
    geom_point(size = 1, data = data.frame(log2FoldChange = 0, logPvalue = 0,
                                           category = factor( c("Not significant", "p-value", "p-value and log2 FC"),
                                                              levels = c("Not significant", "p-value", "p-value and log2 FC"))),
               aes(log2FoldChange, logPvalue, color = category), size = 1, alpha = 0) +
    scale_color_manual(name = "", 
                       values = c("Not significant" = "gray", "p-value" = "blue", "p-value and log2 FC" = "red"), 
                       labels = c("Not significant", "p-value", expression(p-value~and~log[2]~FC))) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 1))) +
    labs(x = expression(log[2]~FC), y = expression(-log[10]~(p-value)), 
         #title = paste0("Week ", list1[[1]][1], ", ", list1[[1]][3], "\nVivarium vs Farm")
    ) + 
    theme_bw() + theme_all
  #ymax = ggplot_build(g)$layout$panel_scales_y[[1]]$range$range[2]
  return(g)
} 

get_gsea_kegg <- function(dds, group1, group2){
  contrast <- c("age_fac_sex", group1, group2)
  res <-  results(dds, contrast = contrast)
  res_shrink <- lfcShrink(dds, contrast = contrast, res = res, type = "ashr")
  res_df <- as.data.frame(res)
  res_df$log2FoldChange_shrink <- res_shrink$log2FoldChange
  
  res_df$ENTREZID <- mapIds(
    org.Mm.eg.db,
    keys      = gsub("\\..*", "", rownames(res_df)),
    column    = "ENTREZID",
    keytype   = "ENSEMBL",
    multiVals = "first"
  )
  
  res_df_list <- res_df |>dplyr::filter(!is.na(ENTREZID), !is.na(log2FoldChange_shrink)) |>
    arrange(desc(log2FoldChange_shrink))
  geneList <- res_df_list$log2FoldChange_shrink
  names(geneList) <- res_df_list$ENTREZID
  geneList <- geneList[!duplicated(names(geneList))]
  
  gsea_kegg <- gseKEGG(geneList = geneList,
                       organism = "mmu",
                       keyType = "ncbi-geneid",
                       pAdjustMethod = "BH",
                       pvalueCutoff  = 0.1,
                       seed = TRUE,
                       eps = 0)
  gsea_kegg
}

