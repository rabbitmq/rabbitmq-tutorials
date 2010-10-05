CONFIG=github.yml

MD_FILES=$(patsubst %.mdx, %.md, $(wildcard */*.mdx))

all: prerequisites _img md

_img:
	mkdir _img

clean:
	rm -f $(MD_FILES) _img/*


md: $(MD_FILES)
java/%.md: java/%.mdx
python/%.md: python/%.mdx
%.md: %.mdx
	BUNDLE_GEMFILE=_tools/Gemfile \
		bundle exec ./_tools/mdxto $(CONFIG) markdown $< > $@


prerequisites:
	make -C _tools prerequisites