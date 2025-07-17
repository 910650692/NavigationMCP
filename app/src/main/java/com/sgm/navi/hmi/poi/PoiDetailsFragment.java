package com.sgm.navi.hmi.poi;


import android.app.Activity;
import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentPoiDetailsBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.user.account.AccessTokenParam;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.base.BaseFragment;

/**
 * @author lvww
 * @version \$Revision1.0\$
 */
@Route(path = RoutePath.Search.POI_DETAILS_FRAGMENT)
public class PoiDetailsFragment extends BaseFragment<FragmentPoiDetailsBinding, PoiDetailsViewModel> {
    private SearchResultEntity mSearchResultEntity;
    private AccessTokenParam mParams;
    private int mViaIndex = -1;
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
        mParams = getAccessTokenParam(getActivity());
    }

    @Override
    public void onInitData() {
        RoutePackage.getInstance().setRouteAlongInfo(null);
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
        try {
            getBundleData();
        }catch (Exception e){
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG,"getBundleData error: "+e.getMessage());
        }
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
            mViaIndex = parsedArgs.getInt(NaviConstant.VIA_POSITION, -1);
            mSearchResultEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA);
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType " , poiType);
            mBinding.scenePoiDetailContentView.refreshPoiView(poiType, poiInfoEntity,true);
            mBinding.scenePoiDetailContentView.setChildIndex(childIndex);
            mBinding.scenePoiDetailContentView.setPowerType(mViewModel.powerType());
            mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
            mBinding.scenePoiDetailContentView.setViaIndexSelect(true,mViaIndex);
            mBinding.scenePoiDetailContentView.setJumpPoiInfo(poiInfoEntity);
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
        final String labelName = parsedArgs.getString("LABEL","");
        final boolean isEnd = parsedArgs.getBoolean("IS_END", false);
        mBinding.scenePoiDetailContentView.refreshPoiView(poiType, poiInfoEntity,true);
        mSearchResultEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA);
        mBinding.scenePoiDetailContentView.doSearch(poiInfoEntity);
        mBinding.scenePoiDetailContentView.setChildIndex(childIndex);
        mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
        mBinding.scenePoiDetailContentView.setLabelName(labelName);
        mBinding.scenePoiDetailContentView.setJumpPoiInfo(poiInfoEntity);
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
                mBinding.scenePoiDetailContentView.refreshPoiView(poiType, poiInfoEntity, true);
                mBinding.scenePoiDetailContentView.setJumpPoiInfo(poiInfoEntity);
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

    public void onSilentSearchResult(final int taskId, final SearchResultEntity searchResultEntity){
        mBinding.scenePoiDetailContentView.onSilentSearchResult(taskId, searchResultEntity);
    }

    public void onNotifyCollectStatus(int taskId){
        mBinding.scenePoiDetailContentView.onCollectUpdate(taskId);
    }

    public void onNetSearchResult() {
        mViewModel.searchCollectList(mParams);
    }

    /**
     * 查询详情失败，通知scene
     * @param taskId 任务id
     * @param message 错误信息
     */
    public void onNetSearchResultError(final int taskId, String message) {
        mBinding.scenePoiDetailContentView.onNetSearchResultError(taskId, message);
    }

    public void onNotifyCollectStatusError(final int taskId, String message){
        mBinding.scenePoiDetailContentView.onNotifyCollectStatusError(taskId, message);
    }

    public void searchReservation(){
        mViewModel.searchReservation(mParams);
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

    @Override
    public void onStop() {
        super.onStop();
        if(!ConvertUtils.isNull(mBinding) && mViaIndex != -1){
            // 取消途径点选中状态
            mBinding.scenePoiDetailContentView.setViaIndexSelect(false,mViaIndex);
            mViaIndex = -1;
        }
    }

    private AccessTokenParam getAccessTokenParam(Activity activity){
        if(!ConvertUtils.isNull(activity)) return null;
        return new AccessTokenParam(
                AutoMapConstant.AccountTokenParamType.ACCOUNT_TYPE_PATAC_HMI,
                AutoMapConstant.AccountTokenParamType.AUTH_TOKEN_TYPE_READ_ONLY,
                null,
                activity,
                null,
                null,
                null,
                null);
    }
}
