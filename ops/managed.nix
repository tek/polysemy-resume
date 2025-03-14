{
  bounds = {
    polysemy-resume = {
      base = {
        lower = "4.16.4.0";
        upper = "4.21";
      };
      incipit-core = {
        lower = "0.4.1.0";
        upper = "0.7";
      };
      polysemy = {
        lower = "1.9.0.0";
        upper = "1.10";
      };
      polysemy-plugin = {
        lower = "0.4.3.0";
        upper = "0.5";
      };
      polysemy-test = {
        lower = "0.6.0.0";
        upper = "0.11";
      };
      stm = {
        lower = "2.5.0.2";
        upper = "2.6";
      };
      tasty = {
        lower = "1.4.0.3";
        upper = "1.6";
      };
      transformers = {
        lower = "0.5.6.2";
        upper = "0.7";
      };
    };
  };
  versions = {
    latest = {
      base = "4.20.0.0";
      incipit-core = "0.6.1.0";
      polysemy = "1.9.2.0";
      polysemy-plugin = "0.4.5.3";
      polysemy-test = "0.10.0.1";
      stm = "2.5.3.1";
      tasty = "1.5.3";
      transformers = "0.6.1.1";
    };
    lower = {
      base = "4.16.4.0";
      incipit-core = "0.4.1.0";
      polysemy = "1.9.0.0";
      polysemy-plugin = "0.4.3.0";
      polysemy-test = "0.6.0.0";
      stm = "2.5.0.2";
      tasty = "1.4.0.3";
      transformers = "0.5.6.2";
    };
  };
  initial = {
    latest = {};
    lower = {
      incipit-core = "0.5.1.0";
      polysemy = "1.9.0.0";
      polysemy-plugin = "0.4.3.0";
      polysemy-test = "0.9.0.0";
      stm = "2.5.0.2";
      tasty = "1.4.0.3";
      transformers = "0.5.6.2";
    };
  };
  overrides = {
    latest = {
      cabal-doctest = {
        version = "1.0.11";
        hash = "152rqpicqpvigjpy4rf1kjlwny1c7ys1r0r123wdjafvv1igflii";
      };
      incipit-base = {
        version = "0.6.1.0";
        hash = "0iyyvxpyyybn5ygr875pav6g5hbs00wa9jbr7qslszqpkfpy5x33";
      };
      incipit-core = {
        version = "0.6.1.0";
        hash = "144c239nxl8zi2ik3ycic3901gxn8rccij3g609n2zgnn3b6zilj";
      };
      polysemy-plugin = {
        version = "0.4.5.3";
        hash = "1c2agk21jj7fwdj6xkagq0prvxknp3zr6q1f480wizssibcvm7y6";
      };
      polysemy-test = {
        version = "0.10.0.1";
        hash = "1sp9iag1brknmdy0qvmgnmynwc4gbg1jy21w584x1m2hpqi25p6j";
      };
      tasty = {
        version = "1.5.3";
        hash = "1xjlmgsww34asjl9rcwbziw5l4qqbvi5l4b7qvzf4dc7hqkpq1rs";
      };
      tasty-hedgehog = {
        version = "1.4.0.2";
        hash = "04kg2qdnsqzzmj3xggy2jcgidlp21lsjkz4sfnbq7b1yhrv2vbbc";
      };
    };
    lower = {
      aeson = {
        version = "2.1.2.1";
        hash = "1f1f6h2r60ghz4p1ddi6wnq6z3i07j60sgm77hx2rvmncz4vizp0";
      };
      assoc = {
        version = "1.1.1";
        hash = "0v4j6bn73dm3xnpkfdx0dg7q4vypl4k31pg35vycfp8w00jv6b6q";
      };
      bifunctors = {
        version = "5.6.2";
        hash = "1g0z6q5z04zgp7kaf917nrj2iiz1lsqh8ji5ny5ly534zr9zya2m";
      };
      cabal-doctest = {
        version = "1.0.9";
        hash = "0irxfxy1qw7sif4408xdhqycddb4hs3hcf6xfxm65glsnmnmwl2i";
      };
      clock = {
        version = "0.8.4";
        hash = "14gy1a16l5s70pyqlsmylxsiiagas2yflqmjjmrdbzj4g1zxy39r";
      };
      comonad = {
        version = "5.0.8";
        hash = "1wwn8z9f3flqlka2k51wqw8wsjcxbp8mwg6yp3vbn6akyjrn36gc";
      };
      incipit-base = {
        version = "0.4.1.0";
        hash = "17579j3hzsh3ic0272h8ly8k7gz4zm1hv5jqimdam9gcq8alahkl";
      };
      incipit-core = {
        version = "0.4.1.0";
        hash = "1fm6bf1w8mvpa9qlgxqv3ngf0lyb3057cwv5ajibgbljjaznxpxc";
      };
      indexed-traversable = {
        version = "0.1.4";
        hash = "061xzz9m77rs6bk5vv2hd7givyq7ln0xffc6m1cxyyhyyr6lw3k0";
      };
      indexed-traversable-instances = {
        version = "0.1.2";
        hash = "05vpkasz70yjf09hsmbw7nap70sr8p5b7hrsdbmij8k8xqf3qg8r";
      };
      path = {
        version = "0.9.5";
        hash = "05b84rizmrii847pq2fbvlpna796bwxha1vc01c3vxb2rhrknrf7";
      };
      path-io = {
        version = "1.8.2";
        hash = "063ma7gzqr5c6s8a1yv72jgll3xdajvgclbc8w0ddmqgcrb62x2k";
      };
      polysemy = {
        version = "1.9.0.0";
        hash = "1af07cppnjpv5v56wanya1mhkvbfnyynf5447mnkcf4zc4k23pyk";
      };
      polysemy-plugin = {
        version = "0.4.3.0";
        hash = "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
      };
      polysemy-test = {
        version = "0.6.0.0";
        hash = "07pi549ral22sxhja67k5b9v787q0b32ysp0bq9szhwjqgxsab46";
      };
      semialign = {
        version = "1.3.1";
        hash = "05h1ab484ghd2wzx4pdlsfwiy6rayy0lzwk9yda9il7fjwi9sj7n";
      };
      semigroupoids = {
        version = "6.0.1";
        hash = "10qd2y5f5m7jzrha1wfbwwybhhghdwkdmk9ajybdz8h88cz9ig2g";
      };
      strict = {
        version = "0.5";
        hash = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
      };
      tasty = {
        version = "1.4.0.3";
        hash = "1cnqrv0k5zkky01pssjy2zm72g73wvqn5c8hj05bq9i9khbbcgkd";
      };
      tasty-hedgehog = {
        version = "1.4.0.2";
        hash = "04kg2qdnsqzzmj3xggy2jcgidlp21lsjkz4sfnbq7b1yhrv2vbbc";
      };
      th-abstraction = {
        version = "0.4.5.0";
        hash = "19nh7a9b4yif6sijp6xns6xlxcr1mcyrqx3cfbp5bdm7mkbda7a9";
      };
      these = {
        version = "1.2.1";
        hash = "0jqchlmycfcvkff48shhkswansnzrw57q8945m483mrd59zpg27k";
      };
      type-errors = {
        version = "0.2.0.2";
        hash = "09rkyqhx8jnzqiq7gpcm5jd1xd435h0ma0b2sff18lk31qv01x6g";
      };
      unbounded-delays = {
        version = "0.1.1.1";
        hash = "1kbh2yr7lwzrhjniyfllsix2zn8bmz9yrkhnq5lxv9ic9bbxnls7";
      };
      wcwidth = {
        version = "0.0.2";
        hash = "0131h9vg8dvrqcc2sn0k8y6cb08fazlfhr4922hwv2vbx3cnyy3z";
      };
      witherable = {
        version = "0.4.2";
        hash = "1ga4al351kwcfvsdr1ngyzj4aypvl46w357jflmgxacad8iqx4ik";
      };
    };
  };
  resolving = false;
}
