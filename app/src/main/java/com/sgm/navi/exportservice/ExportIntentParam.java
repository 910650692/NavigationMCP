package com.sgm.navi.exportservice;

import com.android.utils.log.Logger;
import com.sgm.navi.service.define.search.PoiInfoEntity;

public class ExportIntentParam {

    //三方应用打开Map需要执行的意图，见INaviConstant.OpenIntentPage
    private static int mIntentPage = -1;
    //搜索意图对应的关键字，如果为空，则只是打开搜索页面，不执行搜索操作
    private static String mKeyword;
    //传递过来的poi信息，可作为你地理搜索仅持有经纬度或路线规划终点
    private static PoiInfoEntity mPoiInfo;

    public static int getIntentPage() {
        return mIntentPage;
    }

    public static void setIntentPage(final int intentPage) {
        mIntentPage = intentPage;
        Logger.d("ExportIntentParam", "intentPage : ", mIntentPage);
    }

    public static String getKeyword() {
        return mKeyword;
    }

    public static void setKeyword(final String keyword) {
        mKeyword = keyword;
        Logger.d("ExportIntentParam", "intentPage : ", mKeyword);
    }

    public static PoiInfoEntity getPoiInfo() {
        return mPoiInfo;
    }

    public static void setPoiInfo(final PoiInfoEntity poiInfo) {
        mPoiInfo = poiInfo;
    }
}
