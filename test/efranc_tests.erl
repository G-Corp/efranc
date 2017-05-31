-module(efranc_tests).
-include_lib("eunit/include/eunit.hrl").

efranc_tests_test_() ->
  {setup,
   fun() ->
       ok
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        ?assertEqual(
           "fra",
           efranc:detect("Bonjour le monde, ceci est un premier test avec efranc!")),
        ?assertEqual(
           "kor",
           efranc:detect(
             "한국어 문서가 전 세계 웹에서 차지하는 비중은 2004년에 4.1%로" ++
             "이는 영어(35.8%), 중국어(14.1%), 일본어(9.6%), 스페인어(9%), " ++
             "독일어(7%)에 이어 전 세계 6위이다. 한글 문서와 한국어 문서를 같은" ++
             "것으로 볼 때, 웹상에서의 한국어 사용 인구는 전 세계 69억여 명의 인구 " ++
             "중 약 1%에 해당한다.")),
        ?assertEqual(
           "jpn",
           efranc:detect(
             "現行の学校文法では、英語にあるような「目的語」「補語」" ++
             "などの成分はないとする。英語文法では \"I read a book.\" の " ++
             "\"a book\" はSVO文型の一部をなす目的語であり、また、\"I go to " ++
             "the library.\" の \"the library\" " ++
             "は前置詞とともに付け加えられた修飾語と考えられる。"))
    end,
    fun() ->
        ?assertEqual(undefined, efranc:detect("court")),
        ?assertEqual(undefined, efranc:detect("한국어")),
        ?assertEqual(undefined, efranc:detect("英語文法では")),
        ?assertEqual(undefined, efranc:detect("short")),
        ?assertEqual(undefined, efranc:detect("1234567890"))
    end
   ]}.
