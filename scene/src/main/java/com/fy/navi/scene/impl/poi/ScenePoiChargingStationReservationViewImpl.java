package com.fy.navi.scene.impl.poi;

import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.poi.IScenePoiChargingStationReservationView;
import com.fy.navi.scene.ui.poi.ScenePoiChargingStationReservationView;

public class ScenePoiChargingStationReservationViewImpl extends BaseSceneModel<ScenePoiChargingStationReservationView> implements IScenePoiChargingStationReservationView {
    public ScenePoiChargingStationReservationViewImpl(ScenePoiChargingStationReservationView mScreenView) {
        super(mScreenView);
    }

    public void closeFragment(){
        mScreenView.closeFragment();
    }
}
