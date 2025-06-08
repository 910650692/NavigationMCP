package com.fy.navi.hmi.limit;

import com.fy.navi.service.define.route.RouteRestrictionParam;

/**
 * @author LiuChang
 * @version  \$Revision.1.0\$
 * Date: 2025/2/27
 * Description: [限行政策界面数据缓存]
 */
public class LimitDriverHelper {
    private boolean mNeedClearRestriction = true;
    private RouteRestrictionParam mRoundParam;

    public boolean isNeedClearRestriction() {
        return mNeedClearRestriction;
    }

    public void setNeedClearRestriction(final boolean needClearRestriction) {
        this.mNeedClearRestriction = needClearRestriction;
    }

    public RouteRestrictionParam getRoundParam() {
        return mRoundParam;
    }

    /**
     * 缓存限行政策
     * @param roundParam 限行政策
     */
    public void setRoundParam(final RouteRestrictionParam roundParam) {
        if (roundParam == null) {
            this.mRoundParam = null;
            return;
        }
        this.mRoundParam = new RouteRestrictionParam();
        mRoundParam.setMIsOnlineRoute(roundParam.isMIsOnlineRoute());
        mRoundParam.setMMapTypeId(roundParam.getMMapTypeId());
        mRoundParam.setMRequestId(roundParam.getMRequestId());
        mRoundParam.setMReStrictedAreaResponseParam(roundParam.getMReStrictedAreaResponseParam());
        mRoundParam.setMRestrictedArea(roundParam.getMRestrictedArea());
        mRoundParam.setMRuleIds(roundParam.getMRuleIds());
        mRoundParam.setMRouteRestrictionInfo(roundParam.getMRouteRestrictionInfo());
    }

    public static LimitDriverHelper getInstance() {
        return LimitDriverHelper.Helper.LH;
    }

    private static final class Helper {
        private static final LimitDriverHelper LH = new LimitDriverHelper();
    }
}
