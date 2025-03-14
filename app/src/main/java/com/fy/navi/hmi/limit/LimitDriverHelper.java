package com.fy.navi.hmi.limit;

import com.fy.navi.service.define.route.RouteRestrictionParam;

/**
 * Author: LiuChang
 * Date: 2025/2/27
 * Description: [限行政策界面数据缓存]
 */
public class LimitDriverHelper {
    private boolean needClearRestriction = true;
    private RouteRestrictionParam roundParam;

    public boolean isNeedClearRestriction() {
        return needClearRestriction;
    }

    public void setNeedClearRestriction(boolean needClearRestriction) {
        this.needClearRestriction = needClearRestriction;
    }

    public RouteRestrictionParam getRoundParam() {
        return roundParam;
    }

    public void setRoundParam(RouteRestrictionParam roundParam) {
        this.roundParam = roundParam;
    }

    public static LimitDriverHelper getInstance() {
        return LimitDriverHelper.Helper.lh;
    }

    private static final class Helper {
        private static final LimitDriverHelper lh = new LimitDriverHelper();
    }
}
