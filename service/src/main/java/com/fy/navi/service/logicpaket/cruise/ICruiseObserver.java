package com.fy.navi.service.logicpaket.cruise;

import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface ICruiseObserver {
    /*车道线信息*/
    default void onUpdateCruiseInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
    }

    /*传出巡航探测到的电子眼*/
    default void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
    }

    /*巡航过程中传出巡航状态下的信息*/
    default void onUpdateCruiseInfo(CruiseInfoEntity cruiseInfoEntity) {

    }

    /*导航结束*/
    default void onNaviStop() {
    }
}