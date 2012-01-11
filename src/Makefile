example:ropt.rs
	rustc -o example ropt.rs

lib:
	rustc --lib ropt.rs

testrunner:ropt.rs
	rustc --test -o testrunner ropt.rs

test:testrunner
	./testrunner

clean:
	rm -rf example testrunner libropt.so
