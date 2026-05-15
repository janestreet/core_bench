Exhibit the behavior on [-save]

  $ cd $TEST_DIR
  $ ./core_bench_test_file_name.exe -q 1 -save > /dev/null
  $ ls *.txt | sed -E 's/[-0-9]+/XXX/g'
  file_nameXXXs.txt
