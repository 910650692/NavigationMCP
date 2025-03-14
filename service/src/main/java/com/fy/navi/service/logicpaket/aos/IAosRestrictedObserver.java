package com.fy.navi.service.logicpaket.aos;

import com.fy.navi.service.define.aos.FyCriticism;
import com.fy.navi.service.define.aos.FyGTraEventDetail;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.route.RouteRestrictionParam;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/6
 */
public interface IAosRestrictedObserver {
    // 限行信息获取，注册观察者所需要的key
    public static final String KEY_OBSERVER_LIMIT = "key_observer_limit";
    public static final String KEY_OBSERVER_LIMIT_VIEW = "key_observer_limit_view";

    default void queryLimitResult(RouteRestrictionParam param) {
    }

    default void queryTrafficEventDetailResult(FyGTraEventDetail detail) {
    }

    default void onTrafficUploadFinished(boolean isSuccess) {
    }

    default void  onDynamicPraiseQueryFinished(FyCriticism fyCriticism) {

    }
}
