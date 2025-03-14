package com.fy.navi.service.adapter.cruise.bls;

import com.android.utils.ConvertUtils;
import com.autonavi.gbl.guide.GuideService;
import com.autonavi.gbl.guide.observer.ICruiseObserver;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.cruise.CruiseObserver;


import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class CruiseApiImplHelper {
    private static final String TAG = MapDefaultFinalTag.NAVI_SERVICE_TAG;
    private GuideService mGuideService;
    private final ICruiseObserver cruiseObserver;
    private Hashtable<String, CruiseObserver> mCruiseObservers;

    protected CruiseApiImplHelper(GuideService guideService) {
        this.mGuideService = guideService;
        mCruiseObservers = new Hashtable<>();
        cruiseObserver = new CruiseCallback(mCruiseObservers);
    }

    protected void initCruise() {
        mGuideService.addCruiseObserver(cruiseObserver);
    }

    protected void registerObserver(String key, CruiseObserver cruiseObserver) {
        mCruiseObservers.put(key, cruiseObserver);
    }

    public void unregisterObserver(String key) {
        mCruiseObservers.remove(key);
    }

    protected void unit() {
        mGuideService.removeCruiseObserver(cruiseObserver);
        mCruiseObservers.clear();
    }

    protected void checkNaviService() {
        if (ConvertUtils.equals(ServiceInitStatus.ServiceNotInit, mGuideService.isInit())) {
            initCruise();
        }
    }
}
