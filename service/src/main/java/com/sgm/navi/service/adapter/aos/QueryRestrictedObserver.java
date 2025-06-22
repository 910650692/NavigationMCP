package com.sgm.navi.service.adapter.aos;

import com.sgm.navi.service.define.aos.FyCriticism;
import com.sgm.navi.service.define.aos.FyGTraEventDetail;
import com.sgm.navi.service.define.aos.TrafficRestrictResponseParam;
import com.sgm.navi.service.define.route.RouteRestrictionParam;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/6
 */
public interface QueryRestrictedObserver {
    void onDrawRestrictionAndDetails(RouteRestrictionParam param);

    default void onTrafficRestrict(TrafficRestrictResponseParam trafficRestrictResponseParam) {

    }
    default void onTrafficQueryDetail(FyGTraEventDetail gTraEventDetail) {

    }
    default void onTrafficUploadFinished(boolean isSuccess) {

    }

    default void onDynamicPraiseQueryFinished(FyCriticism fyCriticism) {

    }

    default void onRecvAck(ArrayList<String> data){
    }
}
