all:
	erl -make

clean:
	rm -f *.beam *.dump

run:
	erl -pa ../ebin -boot start_sasl -s test_app -config ./sys.config
