package com.fy.navi.service.adapter.navi.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.guide.GuideService;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;

public class BaseGuideAdapterApiImpl {
    private GuideService mGuideService;

    public GuideService getGuideService() {
        return mGuideService;
    }

    public void setGuideService(final GuideService guideService) {
        this.mGuideService = guideService;
    }

    protected BaseGuideAdapterApiImpl() {
        mGuideService = (GuideService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.GuideSingleServiceID);
        if (mGuideService == null) {
            Logger.e("BaseGuideAdapterApiImpl",
                    "GuideService is null, please check the service registration.");
        }
    }
}
