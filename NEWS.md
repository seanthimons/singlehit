


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

- release v0.2.0
  ([165f457](https://github.com/seanthimons/singlehit/tree/165f457a02e27cfbae540d949dec4b5542c2a486))

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
