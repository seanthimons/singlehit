


## singlehit v0.4.0 (2026-10-29)

#### Breaking changes

- refuse to pool datasets in different dose units
  ([ba62ac2](https://github.com/seanthimons/singlehit/tree/ba62ac22f9855b8045814f59a99db10b3463003d))

#### New features

- explore groups and send pooled data to Fit
  ([b596cb6](https://github.com/seanthimons/singlehit/tree/b596cb68627e793c83519249000885e560fd6c52))
- add 3-row outcome option and batch render for preferred-model overview
  ([bdc1205](https://github.com/seanthimons/singlehit/tree/bdc1205de75692f8adde10002141e2d0dd474e2a))
- add pathogen and outcome faceting modes
  ([d05a7cb](https://github.com/seanthimons/singlehit/tree/d05a7cb5e01734b9d3bb1cbb94464ebfefeb9c9a))

#### Bug fixes

- cap how long one replicate may run
  ([0e50ffe](https://github.com/seanthimons/singlehit/tree/0e50ffee5d1dc5e652ca88de0356a42ba5428ed5))

#### CI

- filter app-only commits out of the release notes
  ([8945ee5](https://github.com/seanthimons/singlehit/tree/8945ee5e294b2956a81d0e2bfc8bb5e57e5d70f2))

#### Docs

- add prerequisites and bootstrapping run-time guidance
  ([f8eacc6](https://github.com/seanthimons/singlehit/tree/f8eacc6f2d8163129470597db9426c10c06b241e))

#### Other changes

- release v0.4.0
  ([16b8211](https://github.com/seanthimons/singlehit/tree/16b8211a47e8bd2f8716f1c9328a6a91d838a700))

Full set of changes:
[`v0.3.0...v0.4.0`](https://github.com/seanthimons/singlehit/compare/v0.3.0...v0.4.0)

## singlehit v0.3.0 (2026-10-20)

#### New features

- add non-blocking bootstrap_progress()
  ([82c147c](https://github.com/seanthimons/singlehit/tree/82c147c13b390712652ced614f6e5dadcb352416))
- add add_bootstrap() to attach bootstraps to an analysis
  ([a2547f1](https://github.com/seanthimons/singlehit/tree/a2547f1c474c519915c8621a60e566bcb5fa892d))
- add exported pooled fit that stacks datasets
  ([0d47431](https://github.com/seanthimons/singlehit/tree/0d47431bee4c73fa8e8ef9a0ba757e46c5cc06b8))

#### Performance

- draw all replicate samples in one vectorized call
  ([c79a061](https://github.com/seanthimons/singlehit/tree/c79a0618d1c1f2ed89ad3b02b72e07fa0f1e54ac))
- stop serializing the task list with every mirai batch
  ([4fac7b4](https://github.com/seanthimons/singlehit/tree/4fac7b40222b1ea86b10fa8c45899c9f8cf04710))

#### Build

- require testthat 3.1.7 for local_mocked_bindings
  ([c88238d](https://github.com/seanthimons/singlehit/tree/c88238d3af4fbb32dea0416965e92b06870007c3))

#### Tests

- cover the detached mirai worker
  ([f08e9cb](https://github.com/seanthimons/singlehit/tree/f08e9cbdb278462ad40d6d7df709043426f9a814))

#### Docs

- explain data frame column mapping and add home TOC
  ([766ff92](https://github.com/seanthimons/singlehit/tree/766ff92f8e6ba356adbaaee2166c094af85ad0a4))

#### Other changes

- release v0.3.0
  ([a5e8633](https://github.com/seanthimons/singlehit/tree/a5e86334a5fb98e403b6ee1b61831cb53325821c))
- block root-level markdown and dot files by default
  ([a23143e](https://github.com/seanthimons/singlehit/tree/a23143e1241a053772222a1dd08e916bda726b18))

Full set of changes:
[`v0.2.0...v0.3.0`](https://github.com/seanthimons/singlehit/compare/v0.2.0...v0.3.0)

## singlehit v0.2.0 (2026-10-01)

#### New features

- preserve experiment metadata and report pooling combinations (#16)
  ([5fbaf27](https://github.com/seanthimons/singlehit/tree/5fbaf27cb4bcb7bbfc1a2a199e993e5dc8827c92))
- add model overlays and pathogen overview
  ([b7807cc](https://github.com/seanthimons/singlehit/tree/b7807cce60d5510edca139d0f07b3ebf876a2ccf))

#### Bug fixes

- batch mirai refits to limit queued jobs (#14)
  ([f54398a](https://github.com/seanthimons/singlehit/tree/f54398ad7d9ea2ce930104afc652639fd2e5d25f))
- show mirai collection progress
  ([ed0c11d](https://github.com/seanthimons/singlehit/tree/ed0c11d96fa001cee840d0876a8b824ef90c85dd))

#### CI

- migrate workflows to baseline v1.3.0 and PR releases (#17)
  ([cd575dd](https://github.com/seanthimons/singlehit/tree/cd575dd616b8822519cf38ea0de2f35a48ad0542))

#### Docs

- fix Ward comparison link and remove premature overview (#15)
  ([49d7858](https://github.com/seanthimons/singlehit/tree/49d78589eed0fc1f9e08293b040a7309ccbce96c))
- index model overlay reference
  ([574f8e8](https://github.com/seanthimons/singlehit/tree/574f8e8df9b0c7bd126b565a13f6dd315c6bc55e))

#### Other changes

- release v0.2.0 (#18)
  ([601da0c](https://github.com/seanthimons/singlehit/tree/601da0cb5f8a774c7408f7510818e3134ddc559c))

Full set of changes:
[`v0.1.0...v0.2.0`](https://github.com/seanthimons/singlehit/compare/v0.1.0...v0.2.0)

## singlehit v0.1.0 (2026-08-20)

#### New features

- automate parallel bootstrap execution
  ([d880429](https://github.com/seanthimons/singlehit/tree/d880429c74fea3fd59fd3bf57739d09de030bf12))
- add dataset pooling via the Haas poolability test
  ([265ce33](https://github.com/seanthimons/singlehit/tree/265ce337dd4df851c4d54a0a7ec473960cf09df1))
- wire the exact beta-Poisson model into the default workflow
  ([8878bf2](https://github.com/seanthimons/singlehit/tree/8878bf2cc60bc264f4668489c3b942c6404ba40a))
- flag and warn on non-converged models in comparison
  ([6d3fbd8](https://github.com/seanthimons/singlehit/tree/6d3fbd87f916f6c77ee8c5c113a594164078eb60))
- generalize model comparison to N models
  ([e9f3a7e](https://github.com/seanthimons/singlehit/tree/e9f3a7ed408efe42eaf8f892d19a3bcc9731bb97))
- fit dose-response models from multiple starts
  ([8541dd6](https://github.com/seanthimons/singlehit/tree/8541dd67adfe6d48e101d5908dd2309637668ad9))
- report chi-squared equivalent in dose_trend_test
  ([05d8111](https://github.com/seanthimons/singlehit/tree/05d8111988cbb14656333c8e29a177c30d99cb8e))
- validate distinct dose-group count and nonzero responses
  ([283f853](https://github.com/seanthimons/singlehit/tree/283f853338b975b1e86511395e1bffc31d0cb690))
- register exact beta-Poisson model in fit and effective_dose
  ([07d9829](https://github.com/seanthimons/singlehit/tree/07d98295b608990fa71375d4bb453e801df7d526))
- add microbial dose-response package workflow
  ([b2fa8da](https://github.com/seanthimons/singlehit/tree/b2fa8da070e00c8c7b615a20bbd8a010ad0fca40))

#### Bug fixes

- remove stale pages during deployment
  ([03f8daf](https://github.com/seanthimons/singlehit/tree/03f8daf0633ddff46facd8280c11991d004e44dd))
- include async bootstrap topics in site index
  ([756c547](https://github.com/seanthimons/singlehit/tree/756c547ea5a636865075baef586913b63f2f8fea))
- default to 10000 replicates to match the CAMRA reference
  ([f79e84f](https://github.com/seanthimons/singlehit/tree/f79e84f1f0c0acad40fa63cace3f3f655b5781d0))
- seed exponential fits with the uncapped ID50 so saturating data
  reaches the global optimum
  ([48e00dc](https://github.com/seanthimons/singlehit/tree/48e00dc1624d28fc984e4e371da430f860d2e465))
- screen data with reading B for the nonzero-response criterion
  ([8c98c37](https://github.com/seanthimons/singlehit/tree/8c98c37200589c93b5d0f31d461457a9c0df7f2b))

#### Refactorings

- reframe dose_trend_test as a directional monotonic pre-screen
  ([868d24c](https://github.com/seanthimons/singlehit/tree/868d24c34fd592d54cddf763bd3fd53cb970a7ed))

#### Tests

- cover bootstrap_confint percentile and filtering logic
  ([3b61658](https://github.com/seanthimons/singlehit/tree/3b616583b3825e359c4451065b9d0dca2c4097db))

#### CI

- disable renv autoloader so pak subprocess can start
  ([c2e7f1c](https://github.com/seanthimons/singlehit/tree/c2e7f1c34bcc6073da4207e6b28e0f0a85714450))
- pin pak to devel to avoid setup-r-dependencies subprocess failures
  ([4e467a7](https://github.com/seanthimons/singlehit/tree/4e467a7f61ea136923fe96352f3e7f3d86131278))
- add package build and release workflows
  ([963a9e8](https://github.com/seanthimons/singlehit/tree/963a9e8a8e18c5c06e7733b44d6d1e735f7475b9))

#### Docs

- reorder README for newcomers and add pkgdown site
  ([79a250f](https://github.com/seanthimons/singlehit/tree/79a250f68279f682791d296408c53866d2dd80d0))
- ship ward_rotavirus dataset and getting-started vignette
  ([8ff731d](https://github.com/seanthimons/singlehit/tree/8ff731db78290338f3d59fe092fb8ec55402a102))
- record audit findings and resolved workflow items in TODO
  ([66a50e7](https://github.com/seanthimons/singlehit/tree/66a50e77337fdb160a1ec2bb6e1b53853a7bf25c))
- move workflow follow-ups to todo
  ([b699c4a](https://github.com/seanthimons/singlehit/tree/b699c4a332325bc9bfd7e1129edb3703bda72890))

#### Other changes

- rename package and repo to singlehit
  ([79c580f](https://github.com/seanthimons/singlehit/tree/79c580f1c03a9bb95b428830929b2439050fef77))
- stop tracking dev/ and the QMRA source workbook
  ([d994dbc](https://github.com/seanthimons/singlehit/tree/d994dbcd2b92b76112f7852251ff5fbcf3c6cc75))
- land v4 validation harness and workbook data-quality review
  ([bf22816](https://github.com/seanthimons/singlehit/tree/bf2281678246541321dbd7d37ac13228c356d4be))
- remove boosterpak and renv scaffolding
  ([b5d90d2](https://github.com/seanthimons/singlehit/tree/b5d90d28c6982138280d45c90243ccaee815d20f))
- remove completed planning and handoff documents
  ([d117b14](https://github.com/seanthimons/singlehit/tree/d117b14f1e5653a2d60c4f1ba205080e80cee4b4))
- ignore dot files generically and document exact-model args
  ([0e06579](https://github.com/seanthimons/singlehit/tree/0e06579fd2839ac55610f197e5a97523707a2a25))

Full set of changes:
[`0c307f5...v0.1.0`](https://github.com/seanthimons/singlehit/compare/0c307f5...v0.1.0)
