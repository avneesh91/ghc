test('linkwhole',
     [extra_files(['Types.hs','Main.hs','MyCode.hs','Handles.hs']),
      when(arch('powerpc64') or arch('powerpc64le'), expect_broken(11259)),
      when(opsys('freebsd'), expect_broken(16035)),
      when(opsys('mingw32'), skip)],
     makefile_test, [])
