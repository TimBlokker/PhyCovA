from __future__ import division, print_function, absolute_import, absolute_import
import numpy as np
from .seq_utils import alphabets


def JTT92(mu=1.0):
    from .gtr import GTR
    # stationary concentrations:
    pis = np.array([
        0.07674789,
        0.05169087,
        0.04264509,
        0.05154407,
        0.01980301,
        0.04075195,
        0.06182989,
        0.07315199,
        0.02294399,
        0.05376110,
        0.09190390,
        0.05867583,
        0.02382594,
        0.04012589,
        0.05090097,
        0.06876503,
        0.05856501,
        0.01426057,
        0.03210196,
        0.06600504])

    # attempt matrix (FIXME)
    Q = np.array([
    [-1.247831,0.044229,0.041179,0.061769,0.042704,0.043467,0.08007,0.136501,0.02059,0.027453,0.022877,0.02669,0.041179,0.011439,0.14794,0.288253,0.362223,0.006863,0.008388,0.227247 ],
    [0.029789,-1.025965,0.023112,0.008218,0.058038,0.159218,0.014895,0.070364,0.168463,0.011299,0.019517,0.33179,0.022599,0.002568,0.038007,0.051874,0.032871,0.064714,0.010272,0.008731 ],
    [0.022881,0.019068,-1.280568,0.223727,0.014407,0.03644,0.024576,0.034322,0.165676,0.019915,0.005085,0.11144,0.012712,0.004237,0.006356,0.213134,0.098304,0.00339,0.029661,0.00678 ],
    [0.041484,0.008194,0.270413,-1.044903,0.005121,0.025095,0.392816,0.066579,0.05736,0.005634,0.003585,0.013316,0.007682,0.002049,0.007682,0.030217,0.019462,0.002049,0.023559,0.015877 ],
    [0.011019,0.022234,0.00669,0.001968,-0.56571,0.001771,0.000984,0.011609,0.013577,0.003345,0.004526,0.001377,0.0061,0.015348,0.002755,0.043878,0.008264,0.022628,0.041124,0.012199 ],
    [0.02308,0.125524,0.034823,0.019841,0.003644,-1.04415,0.130788,0.010528,0.241735,0.003644,0.029154,0.118235,0.017411,0.00162,0.066406,0.021461,0.020651,0.007288,0.009718,0.008098 ],
    [0.064507,0.017816,0.035632,0.471205,0.003072,0.198435,-0.944343,0.073107,0.015973,0.007372,0.005529,0.111197,0.011058,0.003072,0.011058,0.01843,0.019659,0.006143,0.0043,0.027646 ],
    [0.130105,0.099578,0.058874,0.09449,0.042884,0.018898,0.086495,-0.647831,0.016717,0.004361,0.004361,0.019625,0.010176,0.003634,0.017444,0.146096,0.023986,0.039976,0.005815,0.034162 ],
    [0.006155,0.074775,0.089138,0.025533,0.01573,0.1361,0.005927,0.005243,-1.135695,0.003648,0.012767,0.010259,0.007523,0.009119,0.026217,0.016642,0.010487,0.001824,0.130629,0.002508 ],
    [0.01923,0.011752,0.025106,0.005876,0.009081,0.004808,0.00641,0.003205,0.008547,-1.273602,0.122326,0.011218,0.25587,0.047542,0.005342,0.021367,0.130873,0.004808,0.017094,0.513342 ],
    [0.027395,0.0347,0.010958,0.006392,0.021003,0.065748,0.008219,0.005479,0.051137,0.209115,-0.668139,0.012784,0.354309,0.226465,0.093143,0.053877,0.022829,0.047485,0.021916,0.16437 ],
    [0.020405,0.376625,0.153332,0.015158,0.004081,0.170239,0.105525,0.015741,0.026235,0.012243,0.008162,-0.900734,0.037896,0.002332,0.012243,0.027401,0.06005,0.00583,0.004664,0.008162 ],
    [0.012784,0.010416,0.007102,0.003551,0.007339,0.01018,0.004261,0.003314,0.007812,0.113397,0.091854,0.015388,-1.182051,0.01018,0.003788,0.006865,0.053503,0.005682,0.004261,0.076466 ],
    [0.00598,0.001993,0.003987,0.001595,0.031098,0.001595,0.001993,0.001993,0.015948,0.035484,0.098877,0.001595,0.017144,-0.637182,0.006778,0.03668,0.004784,0.021131,0.213701,0.024719 ],
    [0.098117,0.037426,0.007586,0.007586,0.007081,0.082944,0.009104,0.012138,0.058162,0.005058,0.051587,0.010621,0.008092,0.008598,-0.727675,0.144141,0.059679,0.003035,0.005058,0.011632 ],
    [0.258271,0.069009,0.343678,0.040312,0.152366,0.036213,0.020498,0.137334,0.049878,0.02733,0.040312,0.032113,0.019814,0.06286,0.194728,-1.447863,0.325913,0.023914,0.043045,0.025964 ],
    [0.276406,0.037242,0.135003,0.022112,0.02444,0.029677,0.018621,0.019203,0.026768,0.142567,0.014548,0.059936,0.131511,0.006983,0.068665,0.27757,-1.335389,0.006983,0.01222,0.065174 ],
    [0.001275,0.017854,0.001134,0.000567,0.016295,0.002551,0.001417,0.007793,0.001134,0.001275,0.007368,0.001417,0.003401,0.00751,0.00085,0.004959,0.0017,-0.312785,0.010061,0.003542 ],
    [0.003509,0.006379,0.022328,0.014673,0.066664,0.007655,0.002233,0.002552,0.182769,0.010207,0.007655,0.002552,0.005741,0.170967,0.00319,0.020095,0.006698,0.022647,-0.605978,0.005103 ],
    [0.195438,0.011149,0.010493,0.020331,0.040662,0.013117,0.029512,0.030824,0.007214,0.630254,0.11805,0.009182,0.211834,0.040662,0.015084,0.024922,0.073453,0.016396,0.010493,-1.241722]
    ])

    Spis = np.sqrt(pis[None, :] / pis[:,None])
    W = Q * Spis

    gtr = GTR(alphabet=alphabets['aa_nogap'])
    gtr.assign_rates(mu=mu, pi=pis, W=W)
    return gtr
