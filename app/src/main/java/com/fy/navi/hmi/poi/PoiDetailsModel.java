package com.fy.navi.hmi.poi;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

/**
 * @Description POI详情数据处理
 * @Author lvww
 * @date 2024/11/24
 */
public class PoiDetailsModel extends BaseModel<PoiDetailsViewModel> implements SearchResultCallback {
    private final SearchPackage mSearchPackage;
    private final String callbackId;

    public PoiDetailsModel() {
        Logger.d(SEARCH_HMI_TAG, "PoiDetailsModel 初始化");
        mSearchPackage = SearchPackage.getInstance();
        callbackId = UUID.randomUUID().toString();
        mSearchPackage.registerCallBack(callbackId, this);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        if (callbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            Logger.d(SEARCH_HMI_TAG, "搜索结果返回，任务ID：" + taskId);
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                mViewModel.onSearchResult(searchResultEntity);
            }
        } else {
            Logger.d(SEARCH_HMI_TAG, "忽略非自己注册的回调，任务ID：" + taskId);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(SEARCH_HMI_TAG, "PoiDetailsModel 销毁");
        mSearchPackage.unRegisterCallBack(callbackId);
    }
}