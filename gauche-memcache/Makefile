HTML = index.html index.en.html

.SUFFIXES: .html .en.html .scm

%.html: %.scm
	gosh $< ja > $@

%.en.html: %.scm
	gosh $< en > $@

all: $(HTML)

clean:
	@rm -f $(HTML)
