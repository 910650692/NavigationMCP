package com.fy.navi.hmi.poi;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseModel;

import java.util.UUID;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
public class PoiDetailsModel extends BaseModel<PoiDetailsViewModel> implements SearchResultCallback {
    private final SearchPackage mSearchPackage;
    private final String mCallbackId;
    private final CalibrationPackage mCalibrationPackage;
    private final MapPackage mapPackage;
    private double maxDistance = 5; //自车位和地图中心点阈值
    private int mTaskId;
    private SearchResultEntity mSearchResultEntity;

    public PoiDetailsModel() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "PoiDetailsModel 初始化");
        mSearchPackage = SearchPackage.getInstance();
        mCallbackId = UUID.randomUUID().toString();
        mSearchPackage.registerCallBack(mCallbackId, this);
        mCalibrationPackage = CalibrationPackage.getInstance();
        mapPackage= MapPackage.getInstance();
    }

    /**
     * 恢复fragment时，根据数据恢复界面
     */
    public void onReStoreFragment() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "taskId: " + mTaskId + " ,mSearchResultEntity：" + mSearchResultEntity);
        if (!ConvertUtils.isEmpty(mSearchResultEntity)) {
            final ThreadManager threadManager = ThreadManager.getInstance();
            threadManager.postUi(() -> mViewModel.onSearchResult(mTaskId, mSearchResultEntity));
        }
    }

    @Override
    public void onSearchResult(final int taskId, final int errorCode, final String message, final SearchResultEntity searchResultEntity) {
        if (mCallbackId.equals(mSearchPackage.getCurrentCallbackId())) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "搜索结果返回，任务ID：" + taskId);
            if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.POI_SEARCH
                    || searchResultEntity.getSearchType() == AutoMapConstant.SearchType.GEO_SEARCH) {
                final ThreadManager threadManager = ThreadManager.getInstance();
                mTaskId = taskId;
                mSearchResultEntity = searchResultEntity;
                threadManager.postUi(() -> {
                    mViewModel.onSearchResult(taskId, searchResultEntity);
                });
            }
        } else {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "忽略非自己注册的回调，任务ID：" + taskId);
        }
    }

    @Override
    public void onMarkChildClickCallBack(final int index) {
        mViewModel.onMarkChildClickCallBack(index);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "PoiDetailsModel 销毁");
        mSearchPackage.unRegisterCallBack(mCallbackId);
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     * @return 动力类型
     */
    public int powerType() {
        return mCalibrationPackage.powerType();
    }

    /**
     * 判断是否在自车位
     * @return 是否在自车位
     */
    public boolean calcStraightDistance(){
        return mapPackage.isCarLocation(MapType.MAIN_SCREEN_MAIN_MAP,maxDistance);
    }
}