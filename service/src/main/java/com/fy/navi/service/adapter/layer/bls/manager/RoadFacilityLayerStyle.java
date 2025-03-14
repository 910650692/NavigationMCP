package com.fy.navi.service.adapter.layer.bls.manager;

import com.android.utils.ConvertUtils;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizRoadFacilityType;
import com.autonavi.gbl.map.MapView;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/9
 */
public class RoadFacilityLayerStyle extends BaseLayerStyle {

    public RoadFacilityLayerStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
    }


    @Override
    protected void unInit() {
        if (!ConvertUtils.isEmpty(mRoadFacilityControl)) mRoadFacilityControl = null;
    }

    public void setVisibleCruiseSignalLight(boolean isVisible) {
        if (mRoadFacilityControl != null) {
            mRoadFacilityControl.setVisible(BizRoadFacilityType.BizRoadFacilityTypeCruiseTrafficSignalLight, isVisible);
            mRoadFacilityControl.enableLayer(BizRoadFacilityType.BizRoadFacilityTypeCruiseTrafficSignalLight, isVisible);
        }
    }

    public void setVisibleGuideSignalLight(boolean isVisible) {
        if (mRoadFacilityControl != null) {
            mRoadFacilityControl.setVisible(BizRoadFacilityType.BizRoadFacilityTypeGuideTrafficSignalLight, isVisible);
            mRoadFacilityControl.enableLayer(BizRoadFacilityType.BizRoadFacilityTypeGuideTrafficSignalLight, isVisible);
        }
    }
}
