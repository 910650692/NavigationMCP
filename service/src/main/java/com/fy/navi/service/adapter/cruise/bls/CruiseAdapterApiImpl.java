package com.fy.navi.service.adapter.cruise.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.guide.model.NaviType;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.cruise.CruiseObserver;
import com.fy.navi.service.adapter.cruise.ICruiseApi;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navi.bls.BaseGuideAdapterApiImpl;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.logicpaket.layer.LayerPackage;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class CruiseAdapterApiImpl extends BaseGuideAdapterApiImpl implements ICruiseApi {
    private final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private CruiseApiImplHelper cruiseApiImplHelper;
    //引导id,唯一标识
    private long mNaviId;

    public CruiseAdapterApiImpl() {
        super();
        cruiseApiImplHelper = new CruiseApiImplHelper(mGuideService);
    }

    @Override
    public void initCruise() {
        cruiseApiImplHelper.initCruise();
        Logger.d(TAG, "CruiseAdapterApiImpl initNaviService: ");
    }

    @Override
    public void registerObserver(String key, CruiseObserver guidanceObserver) {
        cruiseApiImplHelper.registerObserver(key, guidanceObserver);
    }

    @Override
    public boolean startCruise() {
        mNaviId = NaviConstant.NAVI_CRUISE_ID;
        return mGuideService.startNavi(mNaviId, NaviType.NaviTypeCruise);
    }

    @Override
    public boolean stopCruise() {
        boolean b = mGuideService.stopNavi(mNaviId);
        Logger.i(TAG, "CruiseAdapterApiImpl stopNavigation: " + mNaviId + ",stopNavi：" + b);
        return b;
    }

    @Override
    public void unregisterObserver(String key) {
        cruiseApiImplHelper.unregisterObserver(key);
    }

    @Override
    public void unInitCruise() {
        cruiseApiImplHelper.unit();
    }
}
