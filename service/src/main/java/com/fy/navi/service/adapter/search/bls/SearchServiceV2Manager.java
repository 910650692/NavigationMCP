package com.fy.navi.service.adapter.search.bls;


import com.android.utils.log.Logger;
import com.autonavi.gbl.search.SearchService;
import com.autonavi.gbl.search.SearchServiceV2;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.MapDefaultFinalTag;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 */
public class SearchServiceV2Manager {
    private SearchService mSearchService;
    private SearchServiceV2 mSearchServiceV2;

    /**
     * 初始化服务
     */
    public void initService() {
        if (mSearchService == null) {
            mSearchService = (SearchService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.SearchSingleServiceID);
            mSearchService.init();
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Initialized SearchServiceV1");
        }

        if (mSearchServiceV2 == null) {
            mSearchServiceV2 = (SearchServiceV2) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.SearchV2SingleServiceID);
            mSearchServiceV2.init();
            Logger.d(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Initialized SearchServiceV2");
        }

    }

    /**
     * 获取V1搜索服务
     * @return SearchService
     */
    public SearchService getSearchServiceV1() {
        if (mSearchService == null) {
            initService();
        }
        return mSearchService;
    }

    /**
     * 获取V2搜索服务
     * @return SearchServiceV2
     */
    public SearchServiceV2 getSearchServiceV2() {
        if (mSearchServiceV2 == null) {
            initService();
        }
        return mSearchServiceV2;
    }

    /**
     * 销毁服务
     */
    public void unInit() {
        if (mSearchService != null) {
            mSearchService = null;
        }

        if (mSearchServiceV2 != null) {
            mSearchServiceV2 = null;
        }
    }
}
