# http://robjhyndman.com/hyndsight/makefiles/
# https://www.cs.umd.edu/class/fall2002/cmsc214/Tutorial/makefile.html

# List the R files used
# RFILES := data_prep.R data_analysis.R optimize.R optimization_analysis.R

RFILES := data_prep.R data_analysis.R rp_example.R feasible_space.R pa_opt_views.R

# Rout indicator files to show R file has run
# R CMD BATCH will generate .Rout files after running
OUT_FILES:= $(RFILES:.R=.Rout)


all: $(OUT_FILES) index.html presentation.Rmd

index.html: presentation.Rmd
	Rscript -e "library(methods); library(slidify); slidify('presentation.Rmd')"
	mv presentation.html index.html

# Generate slidy presentation from markdown file
slidy_presentation.html: $(RFILES) $(OUT_FILES) presentation.md
	pandoc -t slidy -s --mathjax presentation.md -o slidy_presentation.html

# Generate slidy presentation from markdown file
slides.pdf: $(RFILES) $(OUT_FILES) presentation.md
	pandoc -t -S beamer —-slide-level 2 presentation.md -o slides.pdf

# Generate markdown file from R markdown file
presentation.md: presentation.Rmd
	Rscript -e "library(knitr); knit('presentation.Rmd')"

# Data prep
data_prep.Rout: data_prep.R
	R CMD BATCH --vanilla data_prep.R

# Data prep
data_analysis.Rout: data_analysis.R
	R CMD BATCH --vanilla data_analysis.R

# Feasible space analysis
feasible_space.Rout: feasible_space.R
	R CMD BATCH --vanilla feasible_space.R

# hierarchical optimization
pa_opt_views.Rout: pa_opt_views.R
	R CMD BATCH --vanilla pa_opt_views.R

# random portfolios example
rp_example.Rout: rp_example.R
	R CMD BATCH --vanilla rp_example.R

# Use Rscript to run the necessary R files as an alternative to R CMD BATCH
runR:
	Rscript data_prep.R
	Rscript data_analysis.R
	Rscript feasible_space.R
	Rscript pa_opt_views.R

clean:
	rm -f *.Rout
	rm -f figures/*.png
	rm -f optimization_results/*.rda
	rm -f presentation.md
	rm -f *.html
	rm -rf libraries/
	rm -rf .cache/
	
