## define the name of source files as below
rmd_source := index.Rmd
output_dir := docs

## corresponding output names
html_out := $(patsubst %.Rmd,docs/%.html,$(rmd_source))
tmp_out := $(patsubst %.Rmd,%.html,$(rmd_source))
tmp_files := $(patsubst %.Rmd,%_files,$(rmd_source))
r_out := $(patsubst %.Rmd,%.R,$(rmd_source))

## CRAN mirror
repos := https://cloud.r-project.org
dep_pkg := revealjs


.PHONY: all
all: $(html_out) $(r_out)

$(html_out): $(rmd_source) _output.yaml
	@$(MAKE) -s check
	@echo "compiling to html slides..."
	@Rscript --vanilla -e "rmarkdown::render('$(rmd_source)')"
	@mv $(tmp_out) docs/
	@rm -rf docs/$(tmp_files)
	@mv $(tmp_files) docs/

$(r_out): $(rmd_source)
	@echo "purling R Markdown source to R script..."
	@Rscript --vanilla -e "knitr::purl('$(rmd_source)')"
	@echo "$(r_out) has been generated."

.PHONY: check
check:
	@Rscript -e \
	"foo <- '$(dep_pkg)' %in% installed.packages()[, 'Package'];" \
	-e "if (! foo) install.packages('$(dep_pkg)', repos = '$(repos)')" \

.PHONY: clean
clean:
	@rm -rf *.aux, *.out *.log *.fls *.fdb_latexmk .Rhistory *\#* .\#* *~

.PHONY: rmCache
rmCache:
	@rm -rf *_cache
