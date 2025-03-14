package com.fy.navi.hmi.favorite;

import com.fy.navi.service.define.route.RouteRestrictionParam;

/**
 * Author: LiuChang
 * Date: 2025/2/27
 * Description: [限行政策界面数据缓存]
 */
public class FavoriteHelper {
    public int getHomeCompanyType() {
        return homeCompanyType;
    }

    public void setHomeCompanyType(int homeCompanyType) {
        this.homeCompanyType = homeCompanyType;
    }

    private int homeCompanyType = -1;


    public static FavoriteHelper getInstance() {
        return FavoriteHelper.Helper.lh;
    }

    private static final class Helper {
        private static final FavoriteHelper lh = new FavoriteHelper();
    }
}
