package com.fy.navi.hmi.poi;


import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentPoiDetailsBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.search.cloudByPatac.rep.BaseRep;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
@Route(path = RoutePath.Search.POI_DETAILS_FRAGMENT)
public class PoiDetailsFragment extends BaseFragment<FragmentPoiDetailsBinding, PoiDetailsViewModel> {
    private SearchResultEntity mSearchResultEntity;
    @Override
    public int onLayoutId() {
        return R.layout.fragment_poi_details;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.scenePoiDetailContentView.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
    }

    @Override
    public void onGetFragmentData() {
        super.onGetFragmentData();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onGetFragmentData " );
        getSearchPoiInfo();
        getBundleData();
    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment " );
        mViewModel.onReStoreFragment();
        getBundleData();
    }

    @Override
    protected void onNewIntent(final Bundle bundle) {
        super.onNewIntent(bundle);
    }

    /**
     * 获取bundle数据
     */
    private void getBundleData() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs != null) {
            final int isOpenFromNavi = parsedArgs.getInt(NaviConstant.NAVI_CONTROL, 0);
            final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
            final int poiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
            final int childIndex = parsedArgs.getInt(AutoMapConstant.ChildIndex.BUNDLE_CHILD_INDEX, -1);
            final boolean isEnd = parsedArgs.getBoolean("IS_END", false);
            mSearchResultEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA);
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType " + poiType);
            mBinding.scenePoiDetailContentView.refreshPoiView(poiType, poiInfoEntity);
            mBinding.scenePoiDetailContentView.setChildIndex(childIndex);
            mBinding.scenePoiDetailContentView.setPowerType(mViewModel.powerType());
            mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
            if (isOpenFromNavi == 1) {
                mBinding.scenePoiDetailContentView.setNaviControl(true);
            }
        }
    }

    /**
     * 获取搜索poi信息
     */
    private void getSearchPoiInfo() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

//        String sourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
        final int poiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
        final int childIndex = parsedArgs.getInt(AutoMapConstant.ChildIndex.BUNDLE_CHILD_INDEX, -1);
        final boolean isEnd = parsedArgs.getBoolean("IS_END", false);
        mBinding.scenePoiDetailContentView.refreshPoiView(poiType, poiInfoEntity);
        mSearchResultEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA);
        mBinding.scenePoiDetailContentView.doSearch(poiInfoEntity);
        mBinding.scenePoiDetailContentView.setChildIndex(childIndex);
        mBinding.scenePoiDetailContentView.setPowerType(mViewModel.powerType());
        mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        Logger.d("onHiddenChanged",hidden);
        super.onHiddenChanged(hidden);
        if (!hidden) {
            if (!ConvertUtils.isEmpty(mSearchResultEntity)) {
                mBinding.scenePoiDetailContentView.reloadLastPoiMarker(mSearchResultEntity.getPoiList());
            }
            final Bundle parsedArgs = getArguments();
            if (parsedArgs != null) {
                final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
                final int poiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
                final boolean isEnd = parsedArgs.getBoolean("IS_END", false);
                mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
                mBinding.scenePoiDetailContentView.refreshPoiView(poiType, poiInfoEntity);
            }
            mBinding.scenePoiDetailContentView.reloadPoiLabelMarker();
            if(mViewModel.calcDistanceBetweenPoints()){
                mBinding.scenePoiDetailContentView.showSelfParkingView();
            }
        }
    }

    /**
     * 搜索结果回调
     * @param taskId 任务id
     * @param searchResultEntity 搜索结果实体类
     */
    public void onSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mBinding.scenePoiDetailContentView.onSearchResult(taskId, searchResultEntity);
    }

    public void onNotifyCollectStatus(BaseRep result){
        mBinding.scenePoiDetailContentView.onCollectUpdate(result.getResultCode());
    }

    public void onNetSearchResult() {
        mViewModel.searchCollectList(getActivity());
    }

    public void searchReservation(){
        mViewModel.searchReservation(getActivity());
    }

    /**
     * 搜索图层子点点击事件
     * @param index 点击下标
     */
    public void onMarkChildClickCallBack(final int index) {
        mBinding.scenePoiDetailContentView.onMarkChildClickCallBack(index);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
