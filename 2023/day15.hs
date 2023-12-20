module Day15 where

import Data.Char (ord, digitToInt)
import qualified Data.Map as Map
import Data.Map (Map, findWithDefault, insert, keys, (!))
import Data.Maybe
import Data.List (findIndex)

type Instruction = (String,Char,Int)
inputTest = ["rn=1","cm-","qp=3","cm=2","qp-","pc=4","ot=9","ab=5","pc-","pc=6","ot=7"]
input = ["vjx=2","nvc=5","xvcn=4","hf-","ggsh=6","tdrm-","ldpk=3","cj-","xbj-","pmlg=8","dcv-","fxk-","lgb-","qk-","dqv=1","bx=7","gcgsd-","gn=7","tdrm-","ftgsl=5","qfcn-","jmb-","sbs-","fz-","xgknn-","fmrlv-","czgkg-","hhs-","gtxp-","zrjb=1","mrr-","gp-","hszks=2","gvs=7","rch-","ll=9","gxvdkh-","mhqgn=7","ccp=5","mnl-","sbs=7","lr-","sqfj=9","st-","hx-","csl-","mhk=4","fz=6","nn=2","vmjt=6","xvc-","hj=3","qk=3","xq=9","mv=6","vtb=8","rm=1","pmlg-","bqv=6","tdtkt=7","xcb=2","xvcn-","ggsh-","kmrr-","kqxnz=4","gh=1","xlgpzc-","sh=6","pgpqv-","gh=8","ncmh=9","bhjt=1","nb=5","brzc-","hs=4","bvf-","qvzz=7","krmg=5","fd=9","bfdc=1","qxr=5","bf=9","gh=3","mbcspc-","hhs-","gks=9","hrg-","xvcn=4","qth-","dn-","ftgsl-","dxlm-","qf-","ckvm=4","fkf-","mlr-","jljc-","xbj=4","xrlb-","pqgr=6","htnls-","snld=6","ndm=2","pmlg=9","dck=3","krm-","rn-","vtb-","zrjb=8","vmjt-","hkv=7","sbjd-","ftgsl=6","gv=3","mch=1","sdtvzs=8","xdrpk-","fkc-","mzp=3","lnk-","flg=9","lmm=9","hjzz=8","xhj-","bhh-","fn-","flg-","gcgsd-","qfcn=8","gzmdj=3","sgss-","vjx-","xs=8","fpmr=3","bhbj=7","xm=7","zr-","jsjg-","mtx-","gzmdj=3","dcv=2","vrz=6","xvc=6","nn-","xm-","ntm-","gnf-","nhp=9","hdf-","zv=9","kzjfcg-","mrxkg=2","vpfh-","ktq-","dlnm-","sd=6","gnk=2","jc=5","phf=6","xxxx-","bcq=8","ccg-","xqkz=2","sgss=7","mnjxx-","xnmz-","ntsj=5","vh-","vfxj-","bk-","snld=1","rlz-","hsl=8","fvj-","xcb=1","mv-","phr=9","hrg=4","zv=4","tl=9","vmjt=6","cl=2","qx-","hzhq-","lpmvvr-","cbz=9","vjdrzb-","gzmdj-","nvkm=2","rk-","vnx=5","mgp=2","ntsj=9","bm=9","nsh-","pg-","zk=9","fkc=1","clz-","vnq=7","plr-","jm-","hb-","tdtkt=4","phf-","nsh=4","hkq=9","cvn=3","bx-","qlcx=9","vbb-","fc=9","nxkjn-","mvl-","mtndqh=6","csms=2","bjqlp-","zr-","vvc-","kqtfg-","gtxp-","pl=9","fq=6","vz=1","gv=4","xx-","csl-","sf=3","tp=2","xt-","dz=6","zj-","jx=5","nz=2","dgp=2","mvl-","mj-","nvc-","vk=2","jd-","dn=2","fq-","rbtnx-","sz-","dzj=7","hl-","gp=7","ntsj-","pgt-","csl=5","zrr-","xd-","zj-","gn=8","hz-","mdt-","hk=7","mtndqh-","nxkjn-","gtxp-","rkpf-","jb-","hj-","xt-","dv=2","lnk-","pmfm-","xjjj-","vpfh=2","htlrt-","gln=5","jp=6","nshdn-","cb=1","xt=8","flg-","tkd-","hkq-","zjzl-","snv-","snld=5","lr=9","ntsj-","tq-","zjzl=1","dcx-","xnzdgf-","xncnhg-","zr=9","bv-","flg=1","npsx-","xlgpzc=3","bhv=9","dk=1","pqgr-","bk=8","xv-","ll-","hvh=1","mqg-","tlqv=9","hvh=6","dzj=6","vldq=6","hbb-","nzr-","ldpk-","dlnm-","fc=5","fj=3","bx=6","zr-","xp=9","mts=7","stm-","vjx-","gcgsd-","rn-","fn-","fj-","mxl=2","xhj-","npsx=6","hbd=9","stm=4","ktkvlc=4","qfcn-","zjzl-","jx=3","vfxj=3","xncnhg=3","bf-","fmrlv-","xcb=6","xs-","bqckr=6","hvh=7","nqg-","mr=9","mnstp=1","nvc-","pl-","kmrr=1","dmh=1","bv-","qk-","mhqgn=3","rkv-","gskf=1","mch-","gskf=6","xz=7","kq-","vrx-","sdns=3","ldpk=1","kz-","bfdc-","hzhq-","ndlb=7","bxcv-","rcrd-","bqv-","sh=8","xqjd=5","sf=3","jmb=9","mxdkf-","mzs-","nhp=6","gnk-","jjq=4","vd=1","rkh-","jc=8","vjx=1","vpfh=4","ccz=2","dmh=3","hszks=1","nb=8","mrxkg=6","rv=8","mrr-","zv=4","kmrr=4","rzl-","ml=7","krmg=1","rp=4","hmd-","xvc-","tb=9","stm=4","bc=2","rkd=2","mfs-","bcq=8","tq-","lmm=4","mjl=1","hmb-","ks-","cg=6","tkd-","mzb-","tb=7","fj=8","rch-","tnrtcx-","krmg=4","rfmb-","mv=9","ccg-","hz=4","xbj-","qrcf=1","zrr-","gjc-","kt=5","jsjg-","ft-","pgt=7","jb=6","htnls=4","fmdb=3","jbzc-","vfbv=9","db=5","dvx=4","bpx=9","xh-","sdns=5","qvb-","db=6","rj=5","qjj-","xmsc-","rcrd=6","ppsql-","lvz-","hbd-","xtk=6","zn-","mlr=5","mzh-","hr=1","qjj-","qp-","xvm=9","krmg-","xlf-","jfbmp=5","ltk=8","lvz=4","ncr=7","rhxg-","fd-","nxkjn-","dcv=6","ndqhp=4","cb=4","bqckr=1","mlcnnh-","hgll=2","xbj-","krmg=8","bvv=4","sp=4","sck-","qdqr-","ntsj=5","dmh=6","gskf=1","qq=1","mfs=9","xv-","ntm-","qpzzx=9","qx-","gz-","nxf=7","mlcnnh-","xmsc=6","vtb=1","xd-","xnmz=1","rkpf-","jfmr-","zjrt=4","zv-","kt=1","bnnxdm=5","jn-","jx=6","bcq-","hk=5","phr-","hhb=5","tg-","vmjt=4","hnfdv=3","rk=5","lgb-","lgrqzb=4","xkptj=3","kczp=7","rtvf=7","ll-","kd-","tz=7","hgll=2","rkpf-","tjd-","qdqr-","hhs=7","fkf-","pvfjl=7","dzs-","fc-","xd-","csms-","qmf=7","qtt=5","rn-","cb=9","xv=8","gln-","xsq-","sbjd-","ls=2","vvk=1","fll=9","cxtn=1","fxk=4","tbcr-","pcr-","xq=4","jv-","dv=6","ndm=6","vrx-","krmg-","gz-","brzc=6","lpmvvr-","dcv=2","qm-","hl-","kmrr-","mzh-","fbp=3","gnk-","bhbj=6","pmfm-","kx-","snv=4","gr=9","cxtn=5","tnrtcx-","mnjxx-","xnmz=3","mzh=1","zxzh=4","fvslps=7","dzj=7","kczp-","fndxj-","hgll=1","xp-","qjj-","qmf=4","mxl-","qtvnt=9","tjzs=2","tb=2","gg-","jljc=7","dcx=1","bd-","bcq=2","szdm=9","nshdn=6","hvf=6","nsh=8","fmxb=9","xv=3","hnd-","gnf-","bjqlp=8","smbb=5","hdf-","nvkm=7","sp=3","nf=7","zrr-","flg=1","kpq=2","bd-","sc=3","pmx-","xq-","cb=2","pvfjl=7","bhbj-","lz=3","xbs=5","qm=6","dn=1","pqbnx-","fmrlv=2","zr-","dsr-","xqjd-","tl-","xmgf=1","zcb-","fll-","gskf=5","gxvdkh-","zcs-","hzhq=7","ft=2","gn-","zcs=6","mzh-","ph-","dkd-","dcx=1","hvh-","zk-","lvz=9","lg=6","dgp-","gh-","bxcv=8","xmsc=1","vt=2","rkd-","zn=1","lfzlkf=9","fkf-","qtdm-","rkd-","mq=8","lz-","nhp-","nqd=9","xgcj=4","nxf-","gsj=4","kmrr=5","ll-","jc=6","lmm=9","jzhvzp=4","qf-","ks-","hx=4","gnf=2","zcb-","nhp-","cgc=8","zjrt=3","bhbj-","ccg=9","bc-","hrg=6","jn-","xqkz=7","tb=2","sp-","tbcr-","mhqgn-","jn=1","rp=9","tz-","vnq=4","nvc-","fmrlv-","vmv=4","qtdm-","db=8","hmd-","dt=4","gnf=9","lg-","zcb-","qx-","jc-","jrd=8","hkv-","bvf-","xvc=2","kpq-","htnls=9","nxf=7","rch-","mjl=3","tz-","xfz-","ccp-","bpx-","nvkm-","lnr=9","fc-","ll=3","xfz=3","jb=7","pgpqv-","dlnm-","kczp=4","hz=3","sk-","mxdkf=6","rv-","xnmz=1","hk=8","zcb-","kd-","mhqgn-","rj-","qvzz=9","jzhvzp-","bx=9","rhxxd=6","jfbmp-","jb=3","lr-","nxf-","nzr-","vzstt-","gjc-","ktq=4","xlp=6","cj=6","jn-","mlr=5","zzpgc-","jljc-","fndxj-","vrx=9","bpx-","bhjt-","jd-","hnpg=5","rfmb=8","mhb-","dsrc-","tmknfh=2","rv-","zj-","nhp-","htnls=8","rv=4","ndm=8","qf=6","nqg-","hq=4","lgrqzb-","gp-","mzs-","xd-","rm-","pqt=7","gg=4","qh=6","xdrpk-","bhh=2","fkc=5","sdns=6","pmlg=8","gv-","rkh=8","fnpxz-","db=8","jnt-","bhbj-","zr-","ld-","rb-","vmv-","xcb=9","qvzz=2","jc=3","tp-","bnnxdm-","mgp-","prq=5","dqst=1","vz-","cf=7","hkc=5","xz=7","ff=1","vp-","nvc-","ckvm-","xm-","rkd-","mqg=3","pgt=2","zv-","rkh=2","mch-","lgb-","fc-","plr=2","vrz-","sbs-","gks=6","ptx-","nqg=3","zdf=2","vp=3","pgv-","zcb=1","lfzlkf-","mdt=6","qm=6","bt-","qj=7","psf=3","fpmr-","gph-","mr-","jsjg=5","xx-","zg=5","fn-","tmknfh=3","hhb-","psf=4","stm-","dt=5","gcgsd=5","tp-","bv=1","lsl-","pmfm-","ktq-","hx=5","ff-","vdp=7","fmdb=6","rtvf-","jflnh-","ndm=4","hnfdv-","ph-","tq-","xlgpzc=7","lnk-","mlcnnh-","cbz-","ns-","ptx=5","lg-","mtndqh-","vvc=6","lmm=3","bxpfng=4","bnnxdm=8","hh-","cq-","nm=8","qnb-","kdlrqg-","dgkzz=6","lsl-","csms-","vjq-","lgb-","xvcn-","zqjxb-","ft-","mq=9","szdm=1","vvc=3","gxvdkh=2","cf=3","mfs=3","dgp-","ktq-","ktt-","sk=8","clz=1","rj-","nxf-","bvf-","kmrr-","zj=7","lpmvvr=2","pqbnx-","gjdj-","qxr=1","ld-","rbtnx=1","zfrck-","cl=8","zrjb-","tg=7","qpzzx=2","tbcr=9","sh=5","gzmdj=1","dk=8","lgb=8","ffl=3","rkh-","zj-","tg-","zz-","dzj=6","bnnxdm-","mj-","lv=8","qh=7","vp=6","kx=5","sq-","qtdm-","rn=6","flg=1","xhj-","bvv-","xlgpzc-","lr-","nshdn-","pgv=2","jfbmp-","dsrc=1","qdqr=4","rp=6","hj=7","vdp-","zg=5","pqgr=9","tm-","rj-","lp=5","kd=6","hq=9","bvf-","qvb-","qvb-","tlqv-","vjnrn=4","kx-","rch-","cl=1","kpq=3","mfs-","gh-","bcq=1","mr=4","mm-","qtvnt-","vf=1","xvm-","jm-","mzp-","mhb-","kb=7","zk-","ndm=5","vldq=3","tg=5","pgfvhl-","rfmb=4","nshdn-","jfbmp-","krmg-","rb-","hnfdv=9","fmhl-","bvv-","ccg-","xgknn-","fp-","szdm-","js-","szdm=9","ndlb=8","ppsql-","rzl=1","mzb=2","dcx=2","jb-","hh-","tplb=6","plr-","kqtfg=6","mhb=3","mlcnnh=9","vnx-","kq-","sbjd-","gg-","bt=9","vjdrzb=9","vfxj=6","vd=6","xvm-","hkv-","pqt=3","dz=1","mlcnnh-","qcr=8","rtj=3","hxmr=3","mfg-","lmm-","kt=4","fvj=7","vzstt-","gxvdkh-","jxt=9","rm-","mq-","brzc=1","nshdn-","xgcj=2","kgdr=2","kpd-","zf-","jljc-","rlq=9","ccz=4","gn-","nbrrr=8","ftq-","qq-","gnk-","pg-","kz-","lsl=9","dgp-","pgv=5","vqp=1","kx=4","ggsh-","kdqg=6","bx=9","zzpgc=6","sbjd=3","bvf-","cvn=5","mjl=6","gh=4","lngv-","ktt-","ll-","dck=4","sf=7","xqkz-","rkv=7","zm-","jfmr=1","czgkg-","bk=4","xrlb-","cg=6","gnf-","xm-","qmf=8","fnpxz=2","rb-","mzp=7","jxt-","ndlb=3","hh-","bqckr=5","vf=1","qcr-","xz-","xvcn-","qvb-","dzj-","vjnrn-","zfrck-","djd-","fc=9","mzp-","hmb-","pj-","vvc=9","ktkvlc=2","sd=1","bv-","txf=4","pmlg=2","jzhvzp=2","qj-","dn-","mhb=3","tp=4","fvj=6","fsgr-","cqgh=7","mtndqh-","fvj-","sbs-","kzjfcg=5","ckm=1","vjx=5","rzmxd=2","pgpqv-","kd-","hrg=5","qvfzm=1","jfmr-","xhj=5","xs=4","ztc=7","ldpk-","jmq=1","hsl=6","bhh-","qrcf=3","clz-","hdf=7","gjdj-","zr=3","jb=8","hnpg-","fj=5","tvsp=7","bx=3","lgrqzb=5","ggv-","nb-","ccp-","gzmdj=7","tq=4","qp=5","ncr-","gc=9","xvm-","hhdf=1","cgc-","pmx-","xt-","zm=4","nhp-","cbz-","sh=3","rlq=7","xmgf=5","fmdb-","xfclgj=2","rbtnx=3","dck=1","fj-","dgkzz=9","hnfdv-","jzhvzp-","jp=9","bhh=9","fmdb-","hkq-","vmv=7","hx=4","mtx-","krmg-","lp-","rlz-","bqv=3","tm=6","jmq=1","gjc-","rtvf=9","mcg-","xgcj-","dz=1","hkc-","vk-","cb-","hj-","vk-","rfmb-","kcx=6","mlcnnh=4","lvz-","vfc-","ckm=2","fxk-","hss=4","xh-","sbjd-","fvj-","zrr=2","qvfzm-","tm=1","bv-","vf=9","fbjtm=5","qpzzx=2","hnfdv-","vjdrzb-","xv-","jrd-","qlcx=4","hs=2","fd-","jljc-","kqtfg=2","gp-","mvl=8","vldq=8","hjzz=6","pg-","pmfm=7","gtxp-","kmrr-","fz=6","pgpqv=2","vvc-","kq=8","zqjxb=4","cb-","dzj=4","hvf-","lck-","pg=3","gcgsd=8","sd-","hjzz=8","ncmh-","htlrt-","ktkq=8","pj=7","ndqhp=2","ndqhp-","ndm=1","gnf-","ks-","vrvf=5","hkq-","fkf-","mtndqh-","rv=2","rtvf-","dlnm-","rzmxd=6","ns=3","mxdkf=1","ll-","kcx=7","pmx-","xvm=9","hbd=2","vp-","bjqlp=9","mm-","pmfm=4","ccp-","zn-","vf-","gr-","dck-","vdp-","gnf-","jpt=8","ntsj=7","djd-","jmb-","ppl=9","jv=7","cxtn-","dn-","plr-","qdqr-","nzr-","zfrck-","nxf=1","ftgsl-","ftlgz-","ckm-","dqv-","xv=8","hhb-","kz-","ks-","lfzlkf=3","cj-","ggv-","mxdkf=1","zrr=5","fb=5","rn-","fll=3","zk-","nvkm-","qpzzx-","tq=8","sqfj=5","rv=1","gv=2","rtj=8","pcr-","dgkzz-","plr=6","pqgr=1","qvfzm-","xgcj=5","rtvf-","lngv-","mrr-","mgp-","rtvf-","lck-","vjq-","rlq-","txf-","mcg-","gnk=7","qx-","qvfzm-","ftq-","rtj-","nml=3","jd=7","mfg-","zcs-","xq-","csms-","hkq=9","mr-","jpt=2","dzs=6","nz=6","dxlm-","vjx=1","dzj=1","hf=5","ntm-","kdlrqg-","fq=6","pgt=3","xqjd=1","jx-","rzmxd=4","dzs=1","gc=2","lgb=9","xjjj=9","ns-","hb-","xxxx-","hgll=5","hj-","flg-","ncr=5","gks=2","xxxx-","hvf=2","flg=8","tz=4","ld-","hk-","dmh-","ls=8","vnx=5","tf=4","ktt=5","fbp-","lngv-","zxzh=6","mqg-","krmg-","stm=9","dk=5","ndlb=6","kq-","nm=7","fmxb=5","gskf-","xjjj=8","hsl=7","nhp-","mcg-","ckm=5","nqg-","xp=5","vldq-","rlq=2","rlz-","vtb=3","tdtkt-","mhqgn=9","qx-","cgc=8","kt=2","lck=8","pmx=2","bnnxdm=8","xncnhg-","rj-","nqd=5","nkk-","tvsp-","cl=8","qnb=9","xlgpzc-","dmh-","bqckr=1","vh-","bvf-","zjrt=9","hk=6","qh=6","qk-","prq-","nb-","bcq=5","vfc-","bvf-","xt-","sc-","st-","mch-","xvc-","ntm-","xz=4","qvb=3","jxt=7","xtk=3","ns-","kd=3","npsx=1","bcq=7","dgkzz-","dmcr=5","tlqv-","hvh=8","zrr=7","htlrt-","mfs-","fvj-","bhh=4","xs-","fkf=3","fmrlv-","mtx-","bxcv=1","pvfjl=3","pg=8","hf-","gg-","vjnrn-","vt-","mxdkf=4","kczp=8","gnk=5","gp=3","qk=5","ft-","vh=7","hss=3","jm-","jx=1","hbd-","tl=9","vnq-","ccp-","ppsql-","mxdkf-","hkv-","fz=5","lngv-","qj=8","zq-","rzmxd=6","lj=4","zg-","lg-","tb=1","hk-","pmx=2","mnjxx-","dgp=7","cvn=4","hdf-","ld-","hvf-","jd-","bt=7","xt=3","ccp=4","xx=5","xd-","ccp-","lsl=3","sk=7","smbb=5","ftq=1","zk-","mrxkg-","lnr=5","fc-","nvkm=1","fxk=3","fll=3","lgb-","dkd=7","gln=5","kd-","cq=2","dgp-","smbb=7","rgrm=5","ns=1","dzs-","hk-","qvzz-","mrr-","xvcn-","cvn=1","hf-","hb=8","vdp=5","xm=1","psf-","gg=7","fn=7","ncmh-","dcx-","fmxb-","rpl=6","hszks-","qtt=5","qxr-","jjq=6","rm-","jnt-","rhxg=4","hzhq=8","tbcr-","mnl-","phf-","vfc-","dkd=7","hmd-","vnx=1","qth=8","bhbj-","mxzhk=2","dxlm=4","fb-","xm-","xlp=6","lfzlkf-","zf-","mr-","cgc-","bqckr-","cbz-","nzr=5","smbb-","fvj-","vjnrn=5","vrvf=1","ph=9","sdns-","rfmb-","xd-","bf-","xnmz-","tjd=7","dv-","gcgsd-","rt=3","rtvf=4","pgpqv-","npsx-","qf-","vdp-","fj-","mzh=7","gph=6","fbjtm-","dz-","vldq-","pqgr-","nbrrr=5","xncnhg=1","hxdst-","ccp-","qtdm-","fmxb-","zdf=3","lr-","mnjxx-","jnt-","jm=2","vt=2","qfcn-","mcg-","fgn-","lnk-","lnr-","jm-","brzc=5","gtxp=4","nf-","lsl=8","mcg=2","ftq-","fvj=4","mrxkg-","xrlb=3","pmx=5","hbb=2","mm=3","tdrm-","sdtvzs=8","ckvm=7","mjl=6","sh=9","xp-","xvm=6","zqjxb=2","hvh=4","xm-","kd=2","hdf-","shczkc=8","pjr-","pgv-","cgc=3","flg-","xqkz-","vqp-","jzhvzp-","ftlgz-","sqfj-","krm-","vz-","hss=6","qcr-","vmv=5","jflnh-","ggsh=8","mqg-","xjjj-","xqkz-","ndlb=8","nxf=3","qth-","mxzhk-","fgn-","xncnhg=5","ftq-","gr=1","vf-","bqckr=2","mrxkg=1","xlp=7","hbd-","xvc-","snld=4","bt-","ccg=7","fpmr-","vbb=6","ncr-","qth=6","mjl-","hb-","xdrpk=9","fbjtm-","rb-","rgrm-","rtj-","mbcspc-","qx=8","mch-","tdrm=7","sbs=2","vqp-","hsl=4","ff-","hkc-","snld-","mqg=1","vfc=9","jbzc=3","qcr=8","hbd-","rv=5","vvc-","vrx-","rkd=4","xq=5","xmsc=9","bt-","zg-","xq=3","pqt=1","fmxb-","xxxx-","dgkzz=2","nvkm-","zqjxb-","zk=3","xfz=6","xvc-","zz-","vldq=3","rkv=2","zdf-","qvzz-","vnq-","ftgsl=8","qnb-","xq-","xgcj=7","ckm=4","hq-","rcrd=4","rch-","hhs-","cgc=8","vbb-","nn=1","hb=3","sc=5","jflnh-","sbjd=3","jtp-","fmhl=3","fd=2","vqp-","mxdkf-","mhqgn-","tjzs-","nml-","lj=7","sp=6","kdlrqg-","xtk-","ntsj-","ktkq-","mxl=7","xnzdgf=5","sp=6","hvf-","tjd-","mv=7","smbb-","fbjtm=1","mvl=6","sgss=5","pl-","rj=2","bhjt-","ffl=9","ppsql=8","qdqr=6","lngv=7","bfdc-","xgknn=4","hxmr=8","dxlm=6","kpd-","xmgf-","dzj=1","sp-","nxf=3","snld-","cb=3","fnpxz-","qdqr-","gnk=5","hdf=9","bvf-","ph-","mqg=3","bcq-","bk=6","pmfm-","vjdrzb-","bhbj-","mnjxx=6","hrg=6","zz-","brzc-","mnl-","hhdf-","vvk-","hz-","fxk-","prq=9","gskf-","bnnxdm=7","phr=6","ztc=9","cbz=4","sk=1","fd=8","bvf=4","xfz-","rk=1","zf=3","cl=9","rpl-","fndxj-","zzpgc=4","kgdr=9","xvm-","nml=2","bhh=8","tz=1","vt=1","pgfvhl-","hg=4","ptx=2","vjq=1","prq-","bhh-","pvfjl=9","fbp-","hjzz=5","vtb-","ns-","jx=5","tnrtcx-","pqgr-","gbp=6","vjnrn=8","ccp=3","lv=5","htnls-","gph-","fpcg=8","mhk-","mvl-","nn=9","hmb-","fll-","jm=1","zjrt=9","zfrck-","hdf-","xrlb=2","fbjtm-","qk=5","rb=9","jpt-","mrxkg=8","xvc=4","hq-","gz-","gks-","vrx-","pjr=5","mqg=9","rhxg-","bhv-","hsl-","hgll-","hg=3","cgc-","lpmvvr-","nhp-","xq=1","nxkjn=6","kzjfcg-","rkh=2","gjdj=3","qf-","js=9","mfs=5","tplb=4","vnx-","fq-","zr=8","xjjj=4","sgss=5","pg=9","fq-","fn-","xgknn=5","mxdkf=4","krm-","vbb=7","js-","gr=4","jflnh-","kpd-","rhxxd-","rp=7","fb-","rzl=7","rzl-","ftlgz-","sck=8","smbb-","bv=6","bjqlp-","ffl=6","xbs-","mcg-","nml-","pg=8","vpfh-","lmn=6","dqv=4","dvx=7","qvb-","lp=2","gcgsd-","hxmr=2","zrjb=2","ffl=9","fxk=6","vtb-","lv=7","jbzc=3","dqst-","qmf=7","kpq-","pmx-","vpfh-","ftq=2","hkv-","rcrd=1","tjd-","mts-","qdqr=3","fnpxz-","gnf=8","kz=2","pl=5","qp=2","gp-","bjqlp=7","tdrm=4","hvh=6","nshdn-","zr-","lr=8","tbcr=6","pgv-","nvc-","xsq-","rlq-","xhj=5","ckvm=7","vfbv-","pgt=2","ggv=1","xkptj-","vrz=3","xt-","dt-","mhb=9","ls=8","xsq=9","zr=9","rv-","zdf-","xp-","tdrm=1","lnr-","qxr=5","tnrtcx=2","hvf=7","kx-","dxlm=1","bfdc=1","vnq=1","nb=8","hxmr=2","ldpk=5","jpt=9","dsrc=6","gg=7","rhxxd-","hj-","nml-","mrxkg-","sdns=7","dmh-","gr=9","qj-","nsh=1","ml-","tbcr=8","xbs=8","rcrd=3","fn-","tbcr=5","qk=6","sf=1","dt-","dt-","vh=8","gkr-","ls=9","xp=9","dz=9","lngv=2","xhj=4","vc=4","htlrt=6","qvb-","dz=6","pqt=5","mch=5","kdlrqg-","kx=9","nshdn=8","xcb=7","tpsp-","xfclgj=5","hbd=7","xfclgj=1","qh-","pj=9","jtp=7","fz=3","hkv-","fj-","gnk-","czgkg-","bbhz-","pgt=3","dzj=8","hkc-","gr=1","ntsj=8","cgc=1","bqckr-","ftlgz=3","mfg-","rbt=6","mbcspc=9","gnf-","lmnn=5","dmcr=7","bqv=6","pmx-","fb-","pvfjl-","vldq=4","bnnxdm-","bv=6","mtndqh=6","qp-","rtvf-","brzc-","fpcg-","vjq=4","fvslps-","sf-","sdns-","jb-","hkc-","jflnh-","fmxb-","vdp-","cqgh-","vjdrzb-","fp-","ztc=4","hf-","xfclgj=4","jmq-","fmxb=6","lp-","tp-","mzs-","fj-","mvl=7","jljc-","cvn-","dgkzz=2","mq=1","cqgh=4","dzj=8","cl=7","zjzl-","pmx=6","ld=8","zn=6","js-","fndxj=4","xxxx=1","xlf=8","mm-","bpx=2","fbp=4","jc=1","dkd=4","fpmr-","ph=2","bhv-","qk-","flg-","nn=4","bvv-","jbzc-","kpd=4","xsq=4","hkq-","lz-","csl=7","tdrm=5","rt=5","hg=1","lj-","rp=2","vrx=5","gnf-","mhb=1","npsx=4","nplk=4","hnfdv-","kqxnz-","ntsj-","kx=7","vfc-","qp-","kczp-","mlcnnh-","kcx=4","xv=7","bhjt-","krmg=1","tl-","jtp-","jxt=3","rgrm-","mr-","mv=1","qtvnt-","nzr=7","lngv=5","ns=6","cbz=3","ks=8","qk=8","hkc-","pvfjl-","gc-","jx-","gp-","shczkc-","cb-","kpq-","vzstt=9","xs-","vz-","cf=6","bf-","pg=5","sgss-","kd=6","tdtkt=9","mjl=7","xdrpk-","pqbnx-","kzjfcg=2","kd-","gh=5","gskf=7","vfbv-","ncmh=2","xqjd-","jjq-","nzr-","jfmr-","mnl-","bhjt=6","vrvf-","ntsj=1","jpt=6","rpl=3","pjr-","kt=3","mqg=1","kdlrqg-","jn=6","smbb-","ntsj-","nvkm=2","gn=7","ft-","shczkc-","zzpgc-","hkq-","mm-","qvfzm=3","rv-","qmf-","rhxxd-","xlp=9","ncmh=3","lsl-","qq=7","fn=7","jkgm=6","sk-","mts=4","rkd=4","mzb-","tg-","cxtn=7","dkd=6","rpl-","qpzzx-","sgss=4","bqv=1","rv=7","npsx-","jxt-","bc=2","qdqr-","qx=7","cq-","qh-","bvf=5","brzc-","jm-","qcr-","pgpqv-","bm=7","bvf=2","tz-","cl-","hdf=1","kpq-","zj=3","qxr=1","rch-","db-","rhxg-","dk=1","mcg=3","stm=4","tm=9","sgss-","qfcn=7","hjzz=1","dsrc=4","vjnrn=2","dzs=3","rzmxd=2","bm=5","xx-","vd=1","bd=3","sdns=9","bqckr=1","mzh-","vldq=8","jxt=6","qrcf-","pjr=4","mhk=2","xt=1","sqfj-","rlq-","tf=1","nqd-","fc=5","kqxnz=3","fsgr-","dvx=2","hkq=4","dxl-","ndlb=9","hvf=7","pgpqv-","prq-","kdqg=9","gn=6","xncnhg-","xjjj-","clz=1","hvf=2","lpmvvr=6","sq=6","fkc=7","psf-","bx-","qfcn-","fkf-","hdf-","vldq-","gp-","kcx-","bv-","qm-","jrd-","vd-","zr-","mrr-","rlz=4","zpt=9","lgb-","ntsj=8","mdt-","vrx=4","lnr-","nkk=3","stm=2","htnls=8","mtndqh=8","lnr=1","pmlg=3","qdqr=4","fbjtm-","jb=1","rhxxd=4","mj=8","fb-","tkd-","xxxx=9","sgss-","mxdkf-","kt=7","jjq-","vqp-","jfmr=8","nplk-","mhk-","hnpg=8","hgll=7","fpcg-","hmb=9","ft=7","jfmr=6","bt-","vvk=6","sc-","vrz=9","rbtnx-","ccz-","hkv-","rv-","rp-","hxdst-","bhbj=6","dt=5","ncmh-","sp=8","gbp-","sck=3","cqgh-","mch-","fd=6","hb=9","rtj-","qf=6","qrcf-","tpsp=5","bfdc=8","mqg=9","jp-","nplk=1","fndxj=2","hkc-","vqp-","bnnxdm=4","ntm=5","bc=2","bqckr-","ckm-","sgss=3","fmdb-","jnt-","nshdn-","tbcr-","hhdf=9","dsr=5","qf=4","xlp-","qvfzm-","ntsj=6","ndlb=9","vjq-","kczp-","vc=9","qmf-","dsrc=2","hh=1","cj=6","qm-","mfg-","dqv-","hh=1","gks-","dt-","rbtnx-","rn=2","ml-","jx-","xz=8","vd=7","nvkm-","dzs=8","xmsc=5","rn-","mcg=1","nvkm=6","lgb=4","kq-","nzr=9","zj=5","lmnn=3","bhbj-","fp=4","cl-","xz=7","zz-","dcv=4","gskf=3","cf-","rbt-","hhs=2","jm-","lgb=4","tq=7","rj-","xfz-","vt=2","dsrc=7","nqd-","vrx-","xmgf-","zm-","hxdst=2","fmxb=1","cl=1","rcrd=9","qvzz=4","pmfm=8","mts-","kdqg=7","zjzl-","tl-","qm-","dxlm-","lngv=5","pl-","gbp-","hgll=1","vjx=5","vz-","ndlb-","rkv-","fq=8","ktkvlc-","xcb-","jx=5","ktkvlc=1","dcx-","rtj=2","gr-","hj=6","ntsj=2","db-","pmfm=2","rtj-","ndm=3","hxmr=5","mqg-","zjzl=1","dsrc-","vrz-","sck-","zdf-","mnstp=3","nml=9","tp-","fq=4","xncnhg=4","kcx-","pcr-","ltk-","mm=4","lg-","ffl-","jd=7","qq=7","vrvf=5","pgfvhl=7","hr-","kgdr=7","szdm-","hkq-","krm=4","mnstp=6","bqv-","fmdb-","csms=7","gnk=8","jbzc=9","gv=7","gcgsd-","dxlm-","lgrqzb=4","hxdst=2","prq=3","mjl-","qk=5","vldq-","ndlb-","qdqr=5","mgp=2","kmrr-","xvc-","ncr-","ntm-","ndlb-","nz-","nb=1","ntm-","zf-","mfg-","cq=3","xtk=6","kgdr=4","xqjd-","hk=5","ncmh-","mtndqh=7","hdf-","vvk=1","gg-","zrr=6","fp=7","gks=4","zz-","pcr-","sq=5","szdm=9","lr-","mzb=4","mxdkf-","fbp-","ktkvlc-","vbb-","htnls-","vnq-","jzhvzp-","cgc=1","pmfm-","kb=5","kdqg-","dgkzz=1","hvf-","qh-","ncmh=4","pmx=4","qjj=1","xhj=9","sgss=6","ndqhp=5","vp=5","fz=8","mgp-","zcs-","pg-","vtb=3","xvcn-","vrvf-","kzjfcg-","tjd-","dt=8","fz=5","rhxg=4","lck-","ks-","vfxj=2","xbj-","kdqg=4","rgrm=5","dvx=8","fsgr-","jzhvzp=9","hj-","lck-","ltk=3","cj=3","qvzz-","jv-","ckvm=6","lv=7","jkgm=9","fd=3","tdrm=6","mjl-","djd-","xgcj=9","qtvnt-","dt-","dgp-","cgc-","jsjg=1","nm=3","bt-","cb-","hhdf-","bd=4","bd-","shczkc-","hl-","nqd-","zxzh-","vmjt=1","kmrr-","fd-","kt-","dqv-","qvfzm=3","fvslps-","ft=8","sdns=7","ktkvlc-","zrjb=9","hmd=8","qfcn=8","zjrt-","mrr=5","hmd-","dzj=5","hmd=1","vvk-","jpt-","bcq-","pl=5","vjnrn=3","dsr=1","jjq=9","hl=8","hf-","cbz-","qf=7","ccp=4","dsr-","ckvm-","hr-","bnnxdm-","ftq-","hh=9","jc-","jtp-","htnls-","bk-","txf=9","rzl=6","phr=5","psf-","cj=3","phf=3","xvc-","st=7","gvs-","ffl-","vrz-","nxkjn=9","ftq-","zz=1","rfmb=3","mdt-","zcs-","gnf-","vrz-","tq-","hz-","bt=9","krmg-","ccp-","jn-","gnf=1","mzb=8","kb=5","nhp-","smg-","nqd=2","lv-","nf-","mxl-","qtdm=3","rzmxd=9","hdf-","jtp-","mj=4","ggv-","bvf=3","dxl-","mcg-","jsjg=7","mrr=7","bbhz=4","dxlm=6","zcb-","qvzz-","fkf-","vzstt=2","ptx-","fmxb-","pgt-","sd=2","zz=7","ppsql=8","rkv-","mnstp-","zrr-","hbb-","hq=4","cq=4","vrvf=3","rv-","mfg-","lj-","lnk=9","hnfdv-","xtk-","hjzz=6","jjq=9","bxpfng=6","rtj=6","tmknfh-","xdrpk=6","jc=7","vvk=5","bjqlp-","vt-","lsl=5","kzjfcg=7","mnstp=6","bxcv=3","rv-","jxt-","ml=7","bqv-","vjnrn-","vmv-","rbtnx=2","fxk-","jflnh=5","gjc-","csl=7","dgkzz-","hkv=4","qnb-","nf=5","hmb=4","nkk-","ntsj=6","xmsc-","jxt-","tmknfh=1","lvz=3","rhxg-","qf=2","jflnh-","cbz-","qk=5","rt-","fb=2","qq-","mbcspc=9","ckm=7","zn=5","rj-","jljc=2","tdtkt-","js=5","jn-","sd=4","pjr=9","xmsc-","qx-","gn-","mxl=2","mtx-","vc-","xd-","rhxg-","tb-","lmn-","zq-","pmx=1","htnls=5","rkd=8","gnf=3","fbjtm-","bvf-","fbjtm=5","nvc=5","ml-","hxmr=5","ndqhp=4","mfg=3","bhbj=8","lvz-","mtx=1","jmb=2","xtk=2","sq-","bvf=6","xtk=6","hgll-","dzj-","bbhz-","mj=2","hq-","fnpxz-","ll-","jjq-","mfg=6","xq-","lvz-","rzl=4","vtb-","phr=2","ktkq-","pvfjl-","mhb=8","sp-","pjr-","hf=1","zg=1","ft-","mtndqh=1","hhs=3","bf-","nvc=2","dqst-","stm=5","zz=5","lr-","djd-","mtx=7","xqjd=2","dqst=9","gr-","lr-","xkptj-","vz=1","zr-","krm=7","rkh=8","nml=6","ztc=7","vfc-","qfcn-","zrr=7","gg=9","sbjd-","gjdj=1","jljc=5","vjdrzb=1","kqxnz-","qvfzm-","kpd=5","vd=9","vmv=3","nqd-","jxt=9","gnf-","fp=5","gz=4","kcx=8","gh=2","hbd=9","kpd=6","tbcr=7","pcr-","gp=5","lp=8","djd=3","kb=7","nn-","fvj-","jfbmp-","lg=3","ztc=9","sbs-","fll=6","lmm=3","xnmz=5","mgp=4","pcr=3","gks=6","jc=3","xxxx=4","ns=2","rgrm-","vc-","hb-","rhxxd=4","vzstt=6","lmn-","gskf-","ppl=7","jnt=1","mch=9","js=5","xgknn-","bfdc-","bk=8","tp=3","xnzdgf=6","gph-","dvx-","bcq-","kpd=2","cvn-","dkd=9","xgcj-","xsq-","vqp=6","qh=1","dkd-","ns-","zv=8","xgcj-","ld-","zdf=6","hhs-","rtvf-","gjc=8","qx=6","pgv=8","mjl-","cxtn=4","vvk=5","zqjxb-","nsh-","sqfj-","hnpg-","hq-","rb-","vmv-","dvx-","bfdc=6","nkk=2","qp-","jn-","qq-","hkc=2","dcv-","vk=9","ftlgz-","clz-","xp=3","mhk=8","snv-","bhjt-","dmh=3","gcgsd-","hh=8","jmq-","rkpf-","smg=5","sd-","vnq=3","ndm-","sq=4","cgc=3","bqv-","pmlg=2","cbz=7","vjnrn-","bvv=9","hl=7","mxdkf-","gp-","nm-","hkv=4","nzr=9","vldq=1","mjl=8","zrjb=6","hrg=4","txf=9","vk-","bhh-","jb=3","qp-","xmsc=9","szdm=8","nbrrr=8","mzb=3","brzc=3","mrr=9","gskf-","mhk=9","dck=1","hs-","phr=5","vvc-","ks-","lp-","hl-","dgp-","hmd-","ccz-","tb-","mr-","pqt=8","htnls=5","fbjtm=5","bxcv=9","dz-","mhk=8","zcb-","fc=9","dv-","smg=5","xsq=9","vjx-","ggv-","hkq=6","vmjt=4","pvfjl=8","hmb-","ccz=6","qq=1","vmjt-","xz-","rbtnx-","hrg-","gg=8","xxxx=6","pjr-","ndlb-","qtvnt-","stm=2","ns=7","mxzhk-","xbj-","xdrpk=8","dlnm-","jd-","lmnn-","rkv-","hx=6","bx=1","lgrqzb-","ztc=5","mdt-","jtp=8","qdqr-","tkd-","fll=7","mcg-","ntm-","lsl=8","flg=9","smg-","phr-","bd-","ltk-","hvh=6","lvz-","qvb-","mrxkg=8","jrd-","fsgr=4","rpl=9","rn-","zf=4","bc-","pqt-","tm=3","xx-","lgb=6","mzp-","lnk-","dv-","nqd=3","mch=5","tplb-","kqtfg-","fmrlv-","fq-","dkd=2","xsq-","dlnm=8","gkr=4","bt=4","bbhz=6","lmn-","kxxkrl=7","jp=1","zm-","rlz=2","hhs-","kxxkrl-","htlrt-","nplk=7","ff=4","fmxb-","sh=7","ftgsl-","gh-","lck-","hhb-","kmrr-","pvfjl=3","zqjxb-","qvb=1","jflnh=2","kxxkrl-","jjq-","nqd=3","pl=8","hbd=2","zzpgc=2","hsl-","cvn=1","sd-","flg-","qcr-","qtt=6","lp-","plr-","vz=2","fvslps=7","rv=8","cq=6","pgpqv-","vz-","mch=8","bxcv-","lvz=8","kdqg=6","ftq-","zv=5","nb=1","mj-","bhjt-","dzj-","fnpxz-","xsq=6","gsj=5","qtt-","hvh-","qcr-","qjj=4","nb-","qcr=8","tjd-","xm-","fc-","mxzhk-","mfg=9","kczp=4","ns-","xdrpk=2","qfcn=5","pmx=1","qp=4","hr-","zj-","zv-","jrd=7","bvv-","hmd-","xs=1","lvz=7","fvslps-","fll=2","shczkc-","hgll-","ndlb-","mzb-","xmgf=2","qtt-","dk=3","jv-","mm-","mhk=2","cb-","mbcspc-","ktkvlc-","kczp=7","zr=8","txf=9","gr=6","kmrr-","hsl=9","nb-","fc-","lv=5","hhs=8","tplb-","lmnn-","csl=2","lpmvvr=9","vbb-","xqjd=6","zz=4","ndlb=5","vt=9","cqgh=7","nsh-","rv=7","mqg-","snld=5","zxzh-","ml=9","ll-","dn=3","dmh-","qh-","jfbmp-","mhb=2","psf-","vjq-","hbb-","vnx=3","pg=2","hxdst=4","mxdkf-","qp=1","ld-","dxlm-","nml-","mvl-","txf-","fq-","zq=9","gtxp-","mcg-","dgp-","hsl=9","bfdc-","rhxg=3","vrz=5","tm=9","fkf=8","fgn-","nm=9","tplb-","fnpxz=8","mqg=5","ff=2","xgknn-","mhk=7","ccg-","ktq-","zv=3","ntsj=7","flg-","hzhq-","dmh-","fn-","rzl=3","xv-","mrxkg-","bjqlp-","nbrrr-","sh=2","bk-","fb-","jpt-","zzpgc-","fvslps-","cgc-","tf=1","ftgsl-","gv-","tpsp=4","hf=5","cgc-","vfxj-","kqxnz=8","lsl=7","htnls-","gskf=7","hrg-","kqtfg=8","ppl-","bpx=8","ph=5","ntm-","rk-","bv-","db-","hj-","zj-","mq=8","ckvm-","kmrr-","fc-","txf=2","hk-","cqgh=8","hs-","mzp-","sq=6","zv=6","xncnhg=2","mm=1","gcgsd-","vp-","fbp=8","qf-","hxdst-","qp=7","hs-","lz-","bt=8","tjd-","ccp-","nqg=5","bt=4","hhb=3","ns-","bxcv=3","tmknfh-","tg=7","rch=1","xv=3","hbb=5","bvv=6","rzmxd-","ktkq-","mdt-","vdp-","hvf-","bqv=9","vfbv-","hk-","hkv=9","vk=4","ldpk=9","xvm-","xm=4","sq-","hmd-","hnfdv-","mch-","dck-","szdm=5","npsx=1","rzl=2","bcq=8","mfg-","dqst-","gjc-","vf=2","jkgm-","hxdst-","krm-","jb=2","mnstp-","qmf-","tl=5","qq=8","gsj-","zrjb=5","gcgsd-","fq=9","mxl-","szdm=3","dgp-","xgknn=1","nbrrr-","bpx=8","gln=3","vbb=4","xfclgj-","sck-","cl-","mzp=6","lg=5","mlr=6","npsx-","xz-","hnpg=1","mlcnnh=9","ftq=6","gr-","jd=4","gxvdkh-","sdtvzs-","zqjxb-","dgp=2","jc=8","qm-","vfxj-","bv-","mgp-","mcg=5","mgp=1","kd=5","gr-","gv=2","fmxb=6","kdlrqg-","rb=1","vfxj=5","hrg-","lg-","psf-","pqbnx-","rv=6","zv=6","bpx=3","jsjg=8","mzp=5","xqkz-","sp=9","nzr-","mq=7","ktkvlc=6","zr-","xm=5","gp-","vc=2","fll=5","jb=1","jmq-","nbrrr-","mxdkf=2","xqjd-","hsl=1","gks-","jljc-","mzs-","dxl-","jbzc-","vrz=6","vjdrzb-","xmgf=4","fpmr-","sz=3","mfg=1","kq=3","gn-","zfrck=4","bcq=7","xlf-","ktkvlc-","ktt=8","lmn-","vtb=5","vh-","hb-","qp-","sz-","tdrm-","gbp=2","bhv-","kzjfcg-","jsjg=2","rp-","plr-","zrjb=5","jn=5","vt-","lmn-","js=1","gskf-","jmq-","mxzhk=4","xnmz=1","rkd=2","qxr-","pgv-","hh=9","nqd=7","hdf=7","kdlrqg=3","gjc=5","mlcnnh-","fkf-","qlcx=3","pcr-","ff=5","jsjg-","hs-","xlf=2","bf-","gskf=6","jljc=2","jtp-","lgrqzb-","kgdr-","htlrt-","xcb=9","nsh=1","sz=1","xmgf=5","dz=4","xjjj-","nxf=3","nz-","cq-","dqst-","xfclgj-","ndm-","cgc-","zjzl-","pgv-","vt-","dck-","ncr-","lnr=6","tq-","flg=8","zcs=6","kt-","xmsc=3","fbp-","dmh-","hss=1","xrlb-","hkv=6","rkv-","dn=1","gjc=7","zzpgc=5","sbs-","sqfj=5","lgrqzb=5","zk=4","xhj=4","mzp-","bhh-","csms=8","vjq=8","pjr-","ml-","shczkc=6","ppsql-","dgkzz=4","fmxb-","mzp-","ncr-","nplk=8","pgt=7","cl=5","qtt-","gr=3","lmnn=3","gbp-","xnmz-","lmn=8","mrxkg-","hkv-","vmjt-","gsj=9","dz=6","gxvdkh=7","htnls=6","vpfh-","pj=2","gskf=5","hrg-","mhk-","mvl=1","hzhq=9","rzmxd=2","fvslps-","fvj=7","st-","zzpgc-","xrlb=1","xp-","cvn-","hgll-","dlnm-","mfs=1","fc=7","mqg=4","gks=6","xkptj-","pvfjl-","jfmr=2","qvzz-","xlp=7","rn-","xqkz-","rp-","zj=2","ktq=8","ft=9","lg-","mbcspc-","dz-","tm=9","fndxj-","sf=6","qjj-","tp-","kzjfcg-","vp=7","hx=4","hxdst=4","dv-","bc=7","mcg=8","bvv-","rb=9","zk=6","qq-","hmd=9","dsr=8","hbd=1","nzr=7","zcb=7","kq=7","xgknn=1","kxxkrl=6","rp=8","gskf-","qfcn-","mtndqh-","mhk-","lmm=5","xm=9","mxzhk-","jnt=3","jx-","sck-","pmx-","fmhl-","fvj-","lg=6","tjzs=2","hnpg=7","nvkm-","bxpfng=7","xnmz=6","lmm-","ktq=3","psf-","hj=7","bjqlp=8","rlq=6","mrr-","lj-","kb-","hjzz-","hzhq=1","rzmxd=5","hkq-","mlr-","zm=9","gjc-","gskf=7","txf=2","jzhvzp=6","gz-","gbp=3","ftlgz=4","zr-","ktkq-","mgp=6","jmq-","rb-","kxxkrl=8","xkptj-","clz-","bqv-","czgkg=1","rn=3","vmv-","zfrck-","gzmdj-","hkc-","bm-","xt-","qx-","xlp=9","mvl-","bxpfng-","vtb=7","tjzs=4","qfcn=7","pmfm=9","mlr-","xdrpk=9","lr-","smg-","pjr-","pgpqv-","jmb=2","ppsql-","bpx=4","vjx-","czgkg=7","xkptj-","vmv=1","zf-","dxl=5","gph=4","hzhq-","ggv=7","tz-","zv=5","sck-","qtt=3","hq-","bxcv-","qh=6","rj-","czgkg-","xbs=6","mvl=3","xd=5","mtx-","js-","fn=4","kpd=1","bhbj=8","ktkvlc-","kmrr=4","htnls=4","xxxx=6","rtj=7","ckm-","mm-","rlz-","xnmz=7","qj-","ggsh-","bqv-","bv-","vjdrzb-","dmcr-","bxcv-","lpmvvr-","zr=9","bpx=6","kb=8","hkq-","ggv=1","ppsql-","qmf=1","gbp-","zqjxb-","fbp-","qrcf-","zz-","tm-","jm=4","xx-","snld-","sh=8","gsj=7","csl=9","gc=3","mfs=1","jmq-","lv=1","lpmvvr=9","vjx-","hhs=3","sf-","djd=4","fz=7","qx=9","qk-","dz=7","vbb-","lnk-","gskf=7","jb-","vvk-","bjqlp=2","xmsc-","fkf=9","ktkq-","bx-","mtndqh=5","tp-","rhxg-","qcr-","htlrt=3","dxlm-","dmh=9","fvj-","csl-","lmn-","bvf-","fq-","ncr-","nqg=6","pg=7","gnf=8","rlz-","mxdkf-","smg-","pgv-","vfbv=2","rfmb-","hz=8","zqjxb=9","xvm-","ppl-","mzs=2","ndm-","tjzs=2","plr=9","dxl-","hs-","zk=9","czgkg=3","tp=9","bf-","jv-","fn=9","pmfm-","pmfm-","ktq-","fmdb=8","xmgf=4","qj=2","mxdkf=7","jflnh-","kq-","qfcn-","nshdn=2","jp=8","tm=4","fmdb-","vqp-","kd=9","pqt-","jm-","jbzc-","qp=8","gp-","ktq=2","rbt-","lgrqzb=5","hx-","hszks=9","ft=9","vrz-","bk-","bk=9","hszks=7","dqv-","mhqgn=3","dmh=9","rtvf-","nml=8","vmv-","krm-","kt-","gg-","lmnn=4","kxxkrl-","zg-","zj-","st-","vrvf=1","vjdrzb=5","vmv-","mgp=5","sck-","rkpf-","rkpf-","xlgpzc-","jp=1","gjc=3","xlgpzc=3","xnmz=8","szdm-","pcr=7","ltk-","gvs=7","ktkvlc-","hnfdv=3","nqd-","tg-","db=7","bk=8","bqckr=7","tp=4","gz=6","pgpqv-","mtx=1","bcq-","ls=3","rkpf=9","djd-","zrr-","nkk=4","ktq=9","ckvm=7","fq-","rtvf=5","mhk=6","hj=4","hxdst=4","qh=4","db=5","mzp=7","bx=5","ndlb=1","hh-","lgb-","zqjxb-","zzpgc-","ccp=7","vc=1","gsj-","tb-","kdlrqg=1","qth-","hvh-","cf-","qmf-","xcb=6","vmjt-","vdp=5","pcr-","phr=3","dzs=2","jxt=2","vjdrzb-","lvz=5","tl-","jc-","xrlb-","qj=7","xgknn-","bd=1","tpsp-","mdt=2","bqckr=3","jv-","zf=1","ltk=4","xqkz=1","csl-","szdm=5","jpt-","dcx-","hrg-","mxl=3","zg=4","ntsj=9","kz=9","dv-","sf=5","vjnrn-","smg-","fgn=1","qtdm-","gkr-","ntsj=5","xcb-","gzmdj=9","ftq-","ff-","zpt-","kqtfg-","hdf=4","vmjt=5","cj=3","jfbmp=3","gr=7","ncr-","dt=1","lmm-","zdf=3","vtb-","tdtkt-","pqgr-","fvslps-","vfxj=5","jrd=9","hsl=1","bvf-","tp=8","hk-","mjl-","ltk=2","nshdn=5","sz=5","sf-","pgfvhl-","nkk=9","bc=8","tg-","xq-","rj=7","qf-","nqd=3","nvkm=1","vjnrn=6","vrvf=4","mrxkg-","lr=5"]

applyChar :: Int -> Char -> Int
applyChar v c = ((v + ord c) * 17) `mod` 256

compute :: String -> Int
compute = foldl applyChar 0

parse :: String -> Instruction
parse s = (v,c,fl)
            where
                r = reverse s
                fl = if not isDash then digitToInt $ head r else 0
                c = if isDash then '-' else '='
                v =  reverse (drop (if isDash then 1 else 2) r)
                isDash = last s == '-'

process :: Map Int [(String, Int)] -> Instruction -> Map Int [(String, Int)]
process m (s, c, fl)    | c == '-' = insert h nlr m
                        | c == '=' && isJust exl = insert h nlu m
                        | c == '=' = insert h nli m
                        | otherwise = error "Erroh"
                        where
                            exl = findIndex (\(cs,_) -> cs == s) cl
                            h = compute s
                            cl = findWithDefault [] h m
                            nlr = filter (\(cs,_) -> cs /= s) cl
                            nlu = replaceNth (fromJust exl) (s, fl) cl
                            nli = cl ++ [(s, fl)]

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs


calculateScore :: Map Int [(String, Int)] -> Int
calculateScore m = sum $ map (calculateBox m) ks
                where
                    ks = keys m

calculateBox :: Map Int [(String, Int)] -> Int -> Int
calculateBox m k = (k + 1) * sum (map (uncurry (*)) tup)
                where
                    tup = zip [1..] (map snd (m ! k))
                    
part1, part2 :: [String] -> Int
part1 i = sum $ map compute i
part2 i = calculateScore computed
        where
            computed = foldl process Map.empty p
            p = map parse i

