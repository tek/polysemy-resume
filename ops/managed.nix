{
  bounds = {
    polysemy-resume = {
      base = {
        lower = "4.17.2.1";
        upper = "4.22";
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
      polysemy-resume = {
        lower = null;
        upper = "0.10";
      };
      polysemy-test = {
        lower = "0.3.1.6";
        upper = "0.12";
      };
      stm = {
        lower = "2.5.1.0";
        upper = "2.6";
      };
      tasty = {
        lower = "1.5.2";
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
      base = "4.21.0.0";
      incipit-core = "0.6.1.1";
      polysemy = "1.9.2.0";
      polysemy-plugin = "0.4.5.3";
      polysemy-test = "0.11.0.1";
      stm = "2.5.3.1";
      tasty = "1.5.4";
      transformers = "0.6.1.2";
    };
    lower = {
      base = "4.17.2.1";
      incipit-core = "0.4.1.0";
      polysemy = "1.9.0.0";
      polysemy-plugin = "0.4.4.0";
      polysemy-test = "0.3.1.6";
      stm = "2.5.1.0";
      tasty = "1.5.2";
      transformers = "0.5.6.2";
    };
  };
  initial = {
    latest = {};
    lower = {
      incipit-core = "0.6.0.0";
      polysemy = "1.9.0.0";
      polysemy-plugin = "0.4.4.0";
      polysemy-test = "0.10.0.1";
      stm = "2.5.1.0";
      tasty = "1.5.2";
      transformers = "0.5.6.2";
    };
  };
  overrides = {
    latest = {
      cabal-doctest = {
        version = "1.0.11";
        hash = "152rqpicqpvigjpy4rf1kjlwny1c7ys1r0r123wdjafvv1igflii";
        repo = "hackage.haskell.org";
      };
      polysemy-test = {
        version = "0.11.0.1";
        hash = "0faajcwslgkjigakimz5sxvcd92p8vdzafway8js8622jmprjqjb";
        repo = "hackage.haskell.org";
      };
      tasty = {
        version = "1.5.4";
        hash = "0x6khif6n0rzfgkvrbiagg1sj0lwmjfr6qarjnjwmb9ywdk7598b";
        repo = "hackage.haskell.org";
      };
      tasty-hedgehog = {
        version = "1.4.0.2";
        hash = "04kg2qdnsqzzmj3xggy2jcgidlp21lsjkz4sfnbq7b1yhrv2vbbc";
        repo = "hackage.haskell.org";
      };
    };
    lower = {
      QuickCheck = {
        version = "2.14.3";
        hash = "18451rdmih1jkrsrckdcix71zqihc4h2caic7qzizxjg4hqpapji";
        repo = "hackage.haskell.org";
      };
      aeson = {
        version = "2.1.2.1";
        hash = "1f1f6h2r60ghz4p1ddi6wnq6z3i07j60sgm77hx2rvmncz4vizp0";
        repo = "hackage.haskell.org";
      };
      async = {
        version = "2.2.6";
        hash = "1731pcifiskq6g1b72p34phx85l65ax7mbjw11310b3zwzk0ldyn";
        repo = "hackage.haskell.org";
      };
      atomic-primops = {
        version = "0.8.8";
        hash = "1razf9zq71am8x1813rrq8m14z6lnrkaacw4zcr5rii6f1q1l6xh";
        repo = "hackage.haskell.org";
      };
      attoparsec = {
        version = "0.14.4";
        hash = "0y9dph5axyvr1bfcvmz6qh50bjcp50m2ljra14960anc6g74a3c8";
        repo = "hackage.haskell.org";
      };
      base-compat = {
        version = "0.13.1";
        hash = "19pgyjgwx81vlyl8c0rlmfcympzy38bsmjq238dn8fv5b03jck4r";
        repo = "hackage.haskell.org";
      };
      base-compat-batteries = {
        version = "0.13.1";
        hash = "06fq90abnlqdsa7iqfmp1fndldmfdfjvx8n8s963nnvnmq6hxs4g";
        repo = "hackage.haskell.org";
      };
      base16-bytestring = {
        version = "1.0.2.0";
        hash = "0r3p5q5zxcrk5cbnwdcm435pgx3yjv3slidpjxdsvigdxizq4fyv";
        repo = "hackage.haskell.org";
      };
      base64-bytestring = {
        version = "1.2.1.0";
        hash = "10gyx24hmlqlvyn7z3kr37w36fkfb6lm4v27sxi233gcfvssxv9r";
        repo = "hackage.haskell.org";
      };
      bifunctors = {
        version = "5.6.3";
        hash = "0hvfzxbj181y61k14fvs5q2vjp1s7s1fay15q6kzvh1b38wg7y0l";
        repo = "hackage.haskell.org";
      };
      cabal-doctest = {
        version = "1.0.12";
        hash = "094mvqgh9bhx5v9xanzkhcm8pcxzmkaa68lr3bqpjzkdxydx81nk";
        repo = "hackage.haskell.org";
      };
      concurrent-output = {
        version = "1.10.21";
        hash = "1w87rrf337s8wc4z3dkh2mk990003jsk18ry5yawv4465k4yvamw";
        repo = "hackage.haskell.org";
      };
      constraints = {
        version = "0.14.4";
        hash = "00cjd15kn30qgq541s0g3sd2lnvrdswx3bkafk0bmrg9b0kdb6hg";
        repo = "hackage.haskell.org";
      };
      data-fix = {
        version = "0.3.4";
        hash = "0x8r2r8gmdvsclaszg90zn7gla6s8r6salbvgfsp0rscdjzj01ry";
        repo = "hackage.haskell.org";
      };
      either = {
        version = "5.0.3";
        hash = "02gw0b0h42qwwlx2h1xk2ayhdw4abl6p1zji75cqs60x85x2a414";
        repo = "hackage.haskell.org";
      };
      ghc-tcplugins-extra = {
        version = "0.4.6";
        hash = "0d0b6h23yq4jccv00wgikxr7hqprkf8afzxcy1mrlgwa9158sl3l";
        repo = "hackage.haskell.org";
      };
      happy = {
        version = "2.2";
        hash = "11xfm7y0dxb676635xqcfgqr0syq9j3hy1157f3kxpb3ljsyg85a";
        repo = "hackage.haskell.org";
      };
      happy-lib = {
        version = "2.2";
        hash = "1j83gcfi1w11p9yb87b543lmkbf3xajyfbid7y2mv0s75jsvqgym";
        repo = "hackage.haskell.org";
      };
      hashable = {
        version = "1.4.4.0";
        hash = "0a8jcfmak3b130x5xim4d6qsqmfp8779wvq5va9irncn6827ihzi";
        repo = "hackage.haskell.org";
      };
      haskell-src-exts = {
        version = "1.23.1";
        hash = "144q88agqqfpc8z1h2jr6mgx5xs72wxkrx4kbpsfg9cza3jm9fbx";
        repo = "hackage.haskell.org";
      };
      haskell-src-meta = {
        version = "0.8.15";
        hash = "0ccwgfkb1n31wwfysdhc1mqpcnnxnczwmz3d4avm9yn9a5m1nh4s";
        repo = "hackage.haskell.org";
      };
      hedgehog = {
        version = "1.7";
        hash = "04cjnz4i1qs3v9bza8a3ry1czapwqgxazhywkjzq2rg1544gjmby";
        repo = "hackage.haskell.org";
      };
      incipit-base = {
        version = "0.4.1.0";
        hash = "17579j3hzsh3ic0272h8ly8k7gz4zm1hv5jqimdam9gcq8alahkl";
        repo = "hackage.haskell.org";
      };
      incipit-core = {
        version = "0.4.1.0";
        hash = "1fm6bf1w8mvpa9qlgxqv3ngf0lyb3057cwv5ajibgbljjaznxpxc";
        repo = "hackage.haskell.org";
      };
      indexed-traversable-instances = {
        version = "0.1.2.1";
        hash = "1issj9yfpxnshm6k7xq3wmmgrhn87cb0jalp0d1ls3zqx0qjrr03";
        repo = "hackage.haskell.org";
      };
      lifted-async = {
        version = "0.11.0";
        hash = "0c0njy8k70swqnp16wyrrkd1bxjsf3pxi34hxka1y1ifp3haccap";
        repo = "hackage.haskell.org";
      };
      path = {
        version = "0.9.5";
        hash = "05b84rizmrii847pq2fbvlpna796bwxha1vc01c3vxb2rhrknrf7";
        repo = "hackage.haskell.org";
      };
      path-io = {
        version = "1.8.2";
        hash = "063ma7gzqr5c6s8a1yv72jgll3xdajvgclbc8w0ddmqgcrb62x2k";
        repo = "hackage.haskell.org";
      };
      polysemy = {
        version = "1.9.0.0";
        hash = "1af07cppnjpv5v56wanya1mhkvbfnyynf5447mnkcf4zc4k23pyk";
        repo = "hackage.haskell.org";
      };
      polysemy-plugin = {
        version = "0.4.4.0";
        hash = "08ry72bw78fis9iallzw6wsrzxnlmayq2k2yy0j79hpw4sp8knmg";
        repo = "hackage.haskell.org";
      };
      polysemy-test = {
        version = "0.3.1.6";
        hash = "0bfh37l68a5chhjfr7gqcffsmvdgg5hqclxi0fc5xnqni2mg81ak";
        repo = "hackage.haskell.org";
      };
      primitive = {
        version = "0.8.0.0";
        hash = "0y8yw8fnfh4dg6yjny2y8b4pmvvhhr7611j2z7i1dnl8w8cvbmb3";
        repo = "hackage.haskell.org";
      };
      profunctors = {
        version = "5.6.3";
        hash = "17daacfx7hmrkcnp2m7c03c171krphh6hwqljvjfhil67sqviclb";
        repo = "hackage.haskell.org";
      };
      relude = {
        version = "1.2.2.2";
        hash = "0nv2cp8wngzi1yszp0pvqi35ii0l63x7y78h5ha938qc8shza2ap";
        repo = "hackage.haskell.org";
      };
      resourcet = {
        version = "1.3.0";
        hash = "0ffmm850z3n95vyj33gfqk606kfn6d50b9bnylqn8y4zrcv5yjmk";
        repo = "hackage.haskell.org";
      };
      safe = {
        version = "0.3.21";
        hash = "1gxpinsixvzb17hzi17yj28fy6n3n97sg3kf4smvf4m9j2alrzbd";
        repo = "hackage.haskell.org";
      };
      scientific = {
        version = "0.3.7.0";
        hash = "09iwj0snmx7vj7x03l4vdcn76zylcgxd9pyz0yxkydgfnn3lvc08";
        repo = "hackage.haskell.org";
      };
      semialign = {
        version = "1.3.1.1";
        hash = "17sfq3kzzdh28vin3kxw6l73jnrawf45cb4rhkcvajhsa9wkwsgv";
        repo = "hackage.haskell.org";
      };
      semigroupoids = {
        version = "6.0.2";
        hash = "0nc2c573inxnp4nz3pbahb66ca9750zdgashwnak7kxyrq7d763l";
        repo = "hackage.haskell.org";
      };
      split = {
        version = "0.2.5";
        hash = "0l9rhscr8rj968qhqhwzf18mddp5d5c22cwwhzb72q646wxmwnh8";
        repo = "hackage.haskell.org";
      };
      strict = {
        version = "0.5";
        hash = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
        repo = "hackage.haskell.org";
      };
      string-interpolate = {
        version = "0.3.4.0";
        hash = "0vvcdd9m7z6rxqcf39xdjaz7hv0hbkkxvcpnyxcvldrsqzlhy3r8";
        repo = "hackage.haskell.org";
      };
      tasty = {
        version = "1.5.2";
        hash = "1jqrcmibqv03109qc6lhi2jnip4mxygcd0j4j0g1n0q0akcplica";
        repo = "hackage.haskell.org";
      };
      tasty-hedgehog = {
        version = "1.4.0.2";
        hash = "04kg2qdnsqzzmj3xggy2jcgidlp21lsjkz4sfnbq7b1yhrv2vbbc";
        repo = "hackage.haskell.org";
      };
      text-conversions = {
        version = "0.3.1.1";
        hash = "0lfcp2f8ld46cry5wm2afcn362mb7fp28ii3afji7bns1fvhh6lf";
        repo = "hackage.haskell.org";
      };
      text-short = {
        version = "0.1.6.1";
        hash = "1yzyzklry9cdc12283b0zf0kpa8nb7gixmdaf3l8x7388zpxhhay";
        repo = "hackage.haskell.org";
      };
      th-abstraction = {
        version = "0.4.5.0";
        hash = "19nh7a9b4yif6sijp6xns6xlxcr1mcyrqx3cfbp5bdm7mkbda7a9";
        repo = "hackage.haskell.org";
      };
      th-expand-syns = {
        version = "0.4.12.0";
        hash = "05p82h3hb7ayidc98qq2bgj790d7km9ixp5ijgc1qqkksg3php6z";
        repo = "hackage.haskell.org";
      };
      th-lift = {
        version = "0.8.4";
        hash = "0rp32lkvx22alxc7c1mxgf224jyanfy93ry70zwdn6zzj50mnbhc";
        repo = "hackage.haskell.org";
      };
      th-orphans = {
        version = "0.13.17";
        hash = "0xzd58ak287rvdf67v8xjxqvx00crpsaa37n447xl85qrpk43fmk";
        repo = "hackage.haskell.org";
      };
      th-reify-many = {
        version = "0.1.10";
        hash = "0g9axz1iszl02cxvy2zgmzinjvz8pbsfq3lzhspshlw5bgcsld39";
        repo = "hackage.haskell.org";
      };
      these = {
        version = "1.2.1";
        hash = "0jqchlmycfcvkff48shhkswansnzrw57q8945m483mrd59zpg27k";
        repo = "hackage.haskell.org";
      };
      time-compat = {
        version = "1.9.9";
        hash = "02yq6qc9fbawpxkypaf4nm9vidfv5vvgidxyj4r3dxa4lb29jd2p";
        repo = "hackage.haskell.org";
      };
      type-errors = {
        version = "0.2.0.2";
        hash = "09rkyqhx8jnzqiq7gpcm5jd1xd435h0ma0b2sff18lk31qv01x6g";
        repo = "hackage.haskell.org";
      };
      unagi-chan = {
        version = "0.4.1.4";
        hash = "1glfzdm732p0zbwq6vg0syw4cg7f72k1982rc6ha8wyr46czdlmm";
        repo = "hackage.haskell.org";
      };
      unordered-containers = {
        version = "0.2.21";
        hash = "0na84q5vxxww3pmz72ihpx4j7dhk71z28r55i7j0pq7mj27jasb0";
        repo = "hackage.haskell.org";
      };
      utf8-string = {
        version = "1.0.2";
        hash = "09i87djx45cinql3mg222m3li08wn8hgmdcp2vqrqc8pn437azrh";
        repo = "hackage.haskell.org";
      };
      uuid-types = {
        version = "1.0.6.1";
        hash = "1jrid43smmfcchrfwpzkxil16a4c5016y4b49yjka0sildj1lprg";
        repo = "hackage.haskell.org";
      };
      vector = {
        version = "0.13.2.0";
        hash = "0mgc7ikhdgqwsj5skdxsf6v3a1iqkiiysqj94qnbg40ff8nbai4x";
        repo = "hackage.haskell.org";
      };
      witherable = {
        version = "0.4.2";
        hash = "1ga4al351kwcfvsdr1ngyzj4aypvl46w357jflmgxacad8iqx4ik";
        repo = "hackage.haskell.org";
      };
    };
  };
  solver = {
    latest = {
      incipit-base = {
        jailbreak = true;
      };
      incipit-core = {
        jailbreak = true;
      };
      polysemy-test = {
        jailbreak = true;
      };
    };
    lower = {};
  };
  packages = {
    polysemy-resume = {
      version = "0.9.0.2";
    };
  };
  resolving = false;
}
