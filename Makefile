stack-all:
	stack --resolver nightly build
	@echo
	stack --resolver lts-16 build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 build
	@echo
	stack --resolver lts-12 --stack-yaml stack-lts12.yaml build
	@echo
	stack --resolver lts-11 --stack-yaml stack-lts12.yaml build
	@echo
	stack --resolver lts-10 --stack-yaml stack-lts12.yaml build
#	@echo optparse-applicative-0.13.2.0 < 0.14.1
#	stack --resolver lts-9 --stack-yaml stack-lts9.yaml build
#	@echo
#	stack --resolver lts-8 --stack-yaml stack-lts10.yaml build
