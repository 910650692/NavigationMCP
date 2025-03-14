package com.fy.navi.service.adapter.aos;

import com.fy.navi.service.define.aos.FyCriticism;
import com.fy.navi.service.define.aos.FyGTraEventDetail;
import com.fy.navi.service.define.route.RouteRestrictionParam;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/6
 */
public interface QueryRestrictedObserver {
    void onDrawRestrictionAndDetails(RouteRestrictionParam param);
    default void onTrafficQueryDetail(FyGTraEventDetail gTraEventDetail) {

    }
    default void onTrafficUploadFinished(boolean isSuccess) {

    }

    default void onDynamicPraiseQueryFinished(FyCriticism fyCriticism) {

    }
}
