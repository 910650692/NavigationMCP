package com.fy.navi.service.adapter.navi.bls;

import com.autonavi.gbl.guide.GuideService;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;

public class BaseGuideAdapterApiImpl {
    protected GuideService mGuideService;

    protected BaseGuideAdapterApiImpl() {
        mGuideService = (GuideService) ServiceMgr.getServiceMgrInstance()
                .getBLService(SingleServiceID.GuideSingleServiceID);
    }
}
