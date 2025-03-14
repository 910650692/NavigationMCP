package com.fy.navi.service.adapter.search.bls;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_SERVICE_TAG;

import com.android.utils.log.Logger;
import com.autonavi.gbl.search.SearchService;
import com.autonavi.gbl.search.SearchServiceV2;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;

/**
 * @Author: baipeng0904
 * @Description: 统一初始化BL搜索服务
 */
public class SearchServiceV2Manager {
    private SearchService mSearchService;
    private SearchServiceV2 mSearchServiceV2;

    public void initService() {
        if (mSearchService == null) {
            mSearchService = (SearchService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.SearchSingleServiceID);
            mSearchService.init();
            Logger.d(SEARCH_SERVICE_TAG, "Initialized SearchServiceV1");
        }

        if (mSearchServiceV2 == null) {
            mSearchServiceV2 = (SearchServiceV2) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.SearchV2SingleServiceID);
            mSearchServiceV2.init();
            Logger.d(SEARCH_SERVICE_TAG, "Initialized SearchServiceV2");
        }

    }

    public SearchService getSearchServiceV1() {
        if (mSearchService == null) {
            initService();
        }
        return mSearchService;
    }

    public SearchServiceV2 getSearchServiceV2() {
        if (mSearchServiceV2 == null) {
            initService();
        }
        return mSearchServiceV2;
    }

    public void unInit() {
        if (mSearchService != null) {
            mSearchService = null;
        }

        if (mSearchServiceV2 != null) {
            mSearchServiceV2 = null;
        }
    }
}
