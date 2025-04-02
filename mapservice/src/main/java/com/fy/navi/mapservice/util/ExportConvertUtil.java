package com.fy.navi.mapservice.util;

import android.util.Log;
import android.util.SparseIntArray;

public final class ExportConvertUtil {

    private static final String TAG = "ExportConvertUtil";

    private SparseIntArray mCityIdArray = new SparseIntArray();

    public static ExportConvertUtil getInstance() {
        return Holder.INSTANCE;
    }

    private static final class Holder {
        private static final ExportConvertUtil INSTANCE = new ExportConvertUtil();
    }

    private ExportConvertUtil() {
        mCityIdArray.put(430100, 158); // 长沙
        mCityIdArray.put(140100, 176); // 太原
        mCityIdArray.put(411300, 309); // 南阳
        mCityIdArray.put(150100, 321); // 呼和浩特
        mCityIdArray.put(431000, 275); // 郴州

        mCityIdArray.put(411600, 308); // 周口
        mCityIdArray.put(330300, 178); // 温州
        mCityIdArray.put(520400, 263); // 安顺
        mCityIdArray.put(410500, 267); // 安阳
        mCityIdArray.put(460100, 125); // 海口

        mCityIdArray.put(652800, 169); // 巴音郭楞   86 库尔勒
        mCityIdArray.put(410700, 152); // 新乡
        mCityIdArray.put(340800, 130); // 安庆
        mCityIdArray.put(130600, 307); // 保定
        mCityIdArray.put(150200, 229); // 包头

        mCityIdArray.put(440300, 340); // 深圳
        mCityIdArray.put(130900, 149); // 沧州
        mCityIdArray.put(330900, 245); // 舟山
        mCityIdArray.put(350100, 300); // 福州
        mCityIdArray.put(320500, 224); // 苏州
        //21
        mCityIdArray.put(220600, 57);  // 白山
        mCityIdArray.put(320400, 348); // 常州
        mCityIdArray.put(652900, 185); // 阿克苏
        mCityIdArray.put(410100, 268); // 郑州
        mCityIdArray.put(510100, 75);  // 成都

        mCityIdArray.put(210200, 167); // 大连
        mCityIdArray.put(341700, 299); // 池州
        mCityIdArray.put(340300, 126); // 蚌埠
        mCityIdArray.put(310000, 289); // 上海
        mCityIdArray.put(441900, 119); // 东莞

        mCityIdArray.put(520500, 227); // 毕节
        mCityIdArray.put(150400, 297); // 赤峰
        mCityIdArray.put(370800, 286); // 济宁
        mCityIdArray.put(341100, 189); // 滁州
        mCityIdArray.put(210600, 282); // 丹东

        mCityIdArray.put(370200, 236); // 青岛
        mCityIdArray.put(371700, 200); // 菏泽
        mCityIdArray.put(533100, 1215);// 德宏   116 芒市
        mCityIdArray.put(370500, 174); // 东营
        mCityIdArray.put(532300, 105); // 楚雄彝族自治州
        //41
        mCityIdArray.put(370100, 288); // 济南
        mCityIdArray.put(512000, 242); // 资阳
        mCityIdArray.put(441400, 141); // 梅州
        mCityIdArray.put(210400, 184); // 抚顺
        mCityIdArray.put(510400, 81);  // 攀枝花

        mCityIdArray.put(360700, 365); // 赣州
        mCityIdArray.put(330100, 179); // 杭州
        mCityIdArray.put(440600, 138); // 佛山
        mCityIdArray.put(530100, 104); // 昆明
        mCityIdArray.put(321000, 346); // 扬州

        mCityIdArray.put(440700, 302); // 江门
        mCityIdArray.put(440100, 257); // 广州
        mCityIdArray.put(520100, 146); // 贵阳
        mCityIdArray.put(230100, 48);  // 哈尔滨
        mCityIdArray.put(510800, 329); // 广元

        mCityIdArray.put(530500, 112); // 保山
        mCityIdArray.put(520300, 262); // 遵义
        mCityIdArray.put(130400, 151); // 邯郸
        mCityIdArray.put(330700, 333); // 金华
        mCityIdArray.put(340100, 127); // 合肥
        //61
        mCityIdArray.put(441300, 301); // 惠州
        mCityIdArray.put(430700, 219); // 常德
        mCityIdArray.put(131100, 208); // 衡水
        mCityIdArray.put(430400, 159); // 衡阳
        mCityIdArray.put(440500, 303); // 汕头

        mCityIdArray.put(532500, 107); // 红河哈尼族彝族自治州
        mCityIdArray.put(330500, 294); // 湖州
        mCityIdArray.put(451100, 260); // 贺州
        mCityIdArray.put(330200, 180); // 宁波
        mCityIdArray.put(370700, 287); // 潍坊
        //71
        mCityIdArray.put(450300, 142); // 桂林
        mCityIdArray.put(320800, 162); // 淮安
        mCityIdArray.put(341000, 252); // 黄山
        mCityIdArray.put(441200, 1216);// 肇庆
        mCityIdArray.put(431200, 363); // 怀化

        mCityIdArray.put(330400, 334); // 嘉兴
        mCityIdArray.put(420600, 274); // 襄阳
        mCityIdArray.put(410800, 211); // 焦作
        mCityIdArray.put(350200, 194); // 厦门
        mCityIdArray.put(140500, 290); // 晋城
        //81
        mCityIdArray.put(360400, 349); // 九江
        mCityIdArray.put(450800, 341); // 贵港
        mCityIdArray.put(410200, 210); // 开封
        mCityIdArray.put(321200, 225); // 泰州靖江  276泰兴
        mCityIdArray.put(440400, 140); // 珠海

        mCityIdArray.put(420200, 311); // 黄石
        mCityIdArray.put(620100, 36);  // 兰州
        mCityIdArray.put(511100, 79);  // 乐山
        mCityIdArray.put(371500, 366); // 聊城
        mCityIdArray.put(450200, 305); // 柳州
        //91
        mCityIdArray.put(421100, 271); // 黄冈
        mCityIdArray.put(440900, 139); // 茂名
        mCityIdArray.put(431300, 221); // 娄底
        mCityIdArray.put(510500, 331); // 泸州
        mCityIdArray.put(410300, 153); // 洛阳

        mCityIdArray.put(411100, 344); // 漯河
        mCityIdArray.put(141100, 327); // 吕梁
        mCityIdArray.put(510700, 240); // 绵阳
        mCityIdArray.put(231000, 49);  // 牡丹江
        mCityIdArray.put(610100, 233); // 西安
        //101
        mCityIdArray.put(360100, 163); // 南昌
        mCityIdArray.put(469002, 2358);// 琼海
        mCityIdArray.put(513400, 80);  // 凉山彝族自治州
        mCityIdArray.put(210300, 320); // 鞍山
        mCityIdArray.put(450100, 261); // 南宁

        mCityIdArray.put(371600, 235); // 滨州
        mCityIdArray.put(320600, 161); // 南通
        mCityIdArray.put(231100, 39);  // 黑河
        mCityIdArray.put(371300, 234); // 临沂
        mCityIdArray.put(510600, 74);  // 德阳
        //111
        mCityIdArray.put(650100, 92);  // 乌鲁木齐
        mCityIdArray.put(211000, 351); // 辽阳
        mCityIdArray.put(620500, 196); // 天水
        mCityIdArray.put(320200, 317); // 无锡
        mCityIdArray.put(520200, 147); // 六盘水

        mCityIdArray.put(360300, 350); // 萍乡
        mCityIdArray.put(410900, 209); // 濮阳
        mCityIdArray.put(522600, 342); // 黔东南苗族侗族自治州
        mCityIdArray.put(130300, 148); // 秦皇岛
        mCityIdArray.put(522300, 343); // 黔西南布依族苗族自治州
        //121
        mCityIdArray.put(130100, 150); // 石家庄
        mCityIdArray.put(331000, 244); // 台州
        mCityIdArray.put(441800, 197); // 清远
        mCityIdArray.put(330800, 243); // 衢州
        mCityIdArray.put(130700, 264); // 张家口

        mCityIdArray.put(530300, 249); // 曲靖
        mCityIdArray.put(350500, 134); // 泉州
        mCityIdArray.put(511300, 291); // 南充
        mCityIdArray.put(421000, 157); // 荆州
        mCityIdArray.put(411400, 154); // 商丘
        //131
        mCityIdArray.put(350400, 254); // 三明
        mCityIdArray.put(350800, 193); // 龙岩
        mCityIdArray.put(440200, 137); // 韶关
        mCityIdArray.put(210100, 58);  // 沈阳
        mCityIdArray.put(522700, 306); // 黔南布依族苗族自治州

        mCityIdArray.put(540100, 100); // 拉萨
        mCityIdArray.put(211300, 280); // 朝阳
        mCityIdArray.put(450700, 145); // 钦州
        mCityIdArray.put(330600, 293); // 绍兴
        mCityIdArray.put(420300, 216); // 十堰
        //141
        mCityIdArray.put(421300, 44);  // 随州
        mCityIdArray.put(610200, 232); // 铜川
        mCityIdArray.put(361100, 364); // 上饶
        mCityIdArray.put(340700, 337); // 铜陵
        mCityIdArray.put(130200, 265); // 唐山

        mCityIdArray.put(430900, 272); // 益阳
        mCityIdArray.put(460200, 121); // 三亚
        mCityIdArray.put(410600, 215); // 鹤壁
        mCityIdArray.put(120000, 332); // 天津
        mCityIdArray.put(511400, 77);  // 眉山
        //151
        mCityIdArray.put(420100, 218); // 武汉
        mCityIdArray.put(150300, 123); // 乌海
        mCityIdArray.put(340200, 129); // 芜湖
        mCityIdArray.put(533300, 113); // 怒江傈僳族自治州
        mCityIdArray.put(630100, 66);  // 西宁

        mCityIdArray.put(450400, 304); // 梧州
        mCityIdArray.put(421200, 362); // 咸宁
        mCityIdArray.put(320300, 316); // 徐州
        mCityIdArray.put(445200, 259); // 揭阳
        mCityIdArray.put(430300, 313); // 湘潭
        //161
        mCityIdArray.put(371000, 175); // 威海
        mCityIdArray.put(360500, 164); // 新余
        mCityIdArray.put(411500, 214); // 信阳
        mCityIdArray.put(420900, 310); // 孝感
        mCityIdArray.put(321300, 62);  // 宿迁

        mCityIdArray.put(130500, 266); // 邢台
        mCityIdArray.put(341800, 190); // 宣城
        mCityIdArray.put(370600, 326); // 烟台
        mCityIdArray.put(222400, 54);  // 延边
        mCityIdArray.put(140300, 357); // 阳泉
        //171
        mCityIdArray.put(410400, 173); // 平顶山汝州  213郏县
        mCityIdArray.put(640200, 335); // 石嘴山
        mCityIdArray.put(654000, 230); // 伊犁哈萨克自治州  90也是伊犁
        mCityIdArray.put(320900, 223); // 盐城
        mCityIdArray.put(430600, 220); // 岳阳

        mCityIdArray.put(360600, 279); // 鹰潭
        mCityIdArray.put(450900, 361); // 玉林
        mCityIdArray.put(445300, 258); // 云浮
        mCityIdArray.put(140800, 328); // 运城
        mCityIdArray.put(610400, 323); // 咸阳
        //181
        mCityIdArray.put(370400, 172); // 枣庄
        mCityIdArray.put(411000, 155); // 许昌
        mCityIdArray.put(350600, 255); // 漳州
        mCityIdArray.put(220100, 53);  // 长春
        mCityIdArray.put(431100, 314); // 永州

        mCityIdArray.put(530400, 106); // 玉溪
        mCityIdArray.put(510300, 78);  // 自贡
        mCityIdArray.put(530600, 336); // 昭通
        mCityIdArray.put(441700, 199); // 阳江
        mCityIdArray.put(321100, 160); // 镇江
        //191
        mCityIdArray.put(360900, 278); // 宜春樟树
        mCityIdArray.put(500000, 132); // 重庆
        mCityIdArray.put(442000, 187); // 中山
        mCityIdArray.put(140400, 356); // 长治
        mCityIdArray.put(370300, 354); // 淄博

        mCityIdArray.put(430200, 222); // 株洲
        mCityIdArray.put(610500, 170); // 渭南
        mCityIdArray.put(130800, 207); // 承德 丰宁
        mCityIdArray.put(131000, 191); // 廊坊
        mCityIdArray.put(140200, 355); // 大同
        //201
        mCityIdArray.put(220700, 52);  // 松原
        mCityIdArray.put(420800, 217); // 荆门
        mCityIdArray.put(140700, 238); // 晋中 寿阳
        mCityIdArray.put(341200, 128); // 阜阳 颍上
        mCityIdArray.put(440800, 198); // 湛江

        mCityIdArray.put(371400, 372); // 德州 武城
        mCityIdArray.put(320700, 347); // 连云港

        Log.d(TAG, "cityId size: " + mCityIdArray.size());
    }

    /**
     * 提供给audio，转换cityId
     * @param cityId 高德cityId
     * @return 百度的cityId,若没有映射默认返回上海的
     */
    public int cityIdConvert(final int cityId) {
        return mCityIdArray.get(cityId, 289);
    }

}
