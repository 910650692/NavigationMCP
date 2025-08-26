package com.sgm.navi.hmi.poi;


import android.app.Activity;
import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
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
    private PoiInfoEntity mPoiInfoEntity;
    private AccessTokenParam mParams;
    private int mViaIndex = -1;
    private boolean mViaUserAdd = true;
    private int mPoiType = AutoMapConstant.PoiType.POI_KEYWORD;
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
        if (mBinding != null) {
            mBinding.scenePoiDetailContentView.setLoadingStatusChangedListener(isLoading -> {
                if (mViewModel != null) {
                    mViewModel.setIsLoading(isLoading);
                }
            });
        }
    }

    @Override
    public void onGetFragmentData() {
        super.onGetFragmentData();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onGetFragmentData " );
        try {
            getBundleData();
            getSearchPoiInfo();
        } catch (Exception e) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "getBundleData error: " + e.getMessage());
        }
    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment " );
        mViewModel.onReStoreFragment();
        try {
            getBundleData();
            if (mBinding != null && mViewModel != null) {
                mBinding.scenePoiDetailContentView.refreshPoiView(mViewModel.getPoiType(), mViewModel.getPoiInfoEntity(),true);
                mBinding.scenePoiDetailContentView.setPoiInfoEntity(mViewModel.getPoiInfoEntity());
                if (mViewModel.getIsLoading() == 1) {
                    mBinding.scenePoiDetailContentView.doSearch(mViewModel.getPoiInfoEntity());
                } else if (mViewModel.getIsLoading() == 2) {
                    mBinding.scenePoiDetailContentView.doTimeoutTask();
                } else {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "current is not in loading status " );
                }
            }
        } catch (Exception e) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "getBundleData error: " + e.getMessage());
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
            mPoiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
            mPoiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
            final int childIndex = parsedArgs.getInt(AutoMapConstant.ChildIndex.BUNDLE_CHILD_INDEX, -1);
            final boolean isEnd = parsedArgs.getBoolean("IS_END", false);
            mViaIndex = parsedArgs.getInt(NaviConstant.VIA_POSITION, -1);
            mViaUserAdd = parsedArgs.getBoolean(NaviConstant.VIA_IS_USER_ADD, true);
            mSearchResultEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA);
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poiType " , mPoiType);
            mBinding.scenePoiDetailContentView.refreshPoiView(mPoiType, mPoiInfoEntity,true);
            mBinding.scenePoiDetailContentView.setChildIndex(childIndex);
            mBinding.scenePoiDetailContentView.setPowerType(mViewModel.powerType());
            mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
            mBinding.scenePoiDetailContentView.setViaIndexSelect(true,mViaIndex);
            mBinding.scenePoiDetailContentView.setViaUserAdd(mViaUserAdd);
            mBinding.scenePoiDetailContentView.setJumpPoiInfo(mPoiInfoEntity);
            if (isOpenFromNavi == 1) {
                mBinding.scenePoiDetailContentView.setNaviControl(true);
            }
            mViewModel.setIsSearchPoiDetailsFragment(mPoiType == AutoMapConstant.PoiType.POI_MAP_CLICK
                    || mPoiType == AutoMapConstant.PoiType.POI_MAP_CAR_CLICK);
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
        mPoiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
        mPoiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
        final int childIndex = parsedArgs.getInt(AutoMapConstant.ChildIndex.BUNDLE_CHILD_INDEX, -1);
        final String labelName = parsedArgs.getString("LABEL","");
        final boolean isEnd = parsedArgs.getBoolean("IS_END", false);
        mBinding.scenePoiDetailContentView.refreshPoiView(mPoiType, mPoiInfoEntity,true);
        mSearchResultEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_SOURCE_DATA);
        mBinding.scenePoiDetailContentView.doSearch(mPoiInfoEntity);
        mBinding.scenePoiDetailContentView.setChildIndex(childIndex);
        mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
        mBinding.scenePoiDetailContentView.setLabelName(labelName);
        mBinding.scenePoiDetailContentView.setJumpPoiInfo(mPoiInfoEntity);
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        Logger.d("onHiddenChanged", hidden);
        super.onHiddenChanged(hidden);
        if (!hidden) {
            try {
                ThreadManager.getInstance().postDelay(reloadRunnable, 200);
                final Bundle parsedArgs = getArguments();
                if (parsedArgs != null) {
                    mPoiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL);
                    mPoiType = parsedArgs.getInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
                    final boolean isEnd = parsedArgs.getBoolean("IS_END", false);
                    mBinding.scenePoiDetailContentView.setIsEnd(isEnd);
                    mBinding.scenePoiDetailContentView.refreshPoiView(mPoiType, mPoiInfoEntity, true);
                    mBinding.scenePoiDetailContentView.setJumpPoiInfo(mPoiInfoEntity);
                }
                if (mViewModel.calcDistanceBetweenPoints()) {
                    mBinding.scenePoiDetailContentView.showSelfParkingView();
                }
            } catch (Exception e) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "getBundleData error: " + e.getMessage());
            }
        }
    }

    private final Runnable reloadRunnable = new Runnable() {
        @Override
        public void run() {
            if (mBinding != null) {
                if (!ConvertUtils.isEmpty(mSearchResultEntity) && !ConvertUtils.isEmpty(mBinding)) {
                    mBinding.scenePoiDetailContentView.reloadLastPoiMarker(mSearchResultEntity.getPoiList());
                    mBinding.scenePoiDetailContentView.reloadPoiLabelMarker();
                }
            }
        }
    };

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
    public void onDestroyView() {
        super.onDestroyView();
        if (mViewModel != null && mBinding != null) {
            mViewModel.setSearchResultEntity(mBinding.scenePoiDetailContentView.getSearchResultEntity());

        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mViewModel != null) {
            mViewModel.setPoiInfoEntity(mPoiInfoEntity);
            mViewModel.setPoiType(mPoiType);
        }
    }

    @Override
    public void onStop() {
        super.onStop();
        if(!ConvertUtils.isNull(mBinding) && mViaIndex != -1){
            // 取消途径点选中状态
            mBinding.scenePoiDetailContentView.setViaIndexSelect(false,mViaIndex);
            mViaIndex = -1;
        }
        ThreadManager.getInstance().removeHandleTask(reloadRunnable);
    }

    private AccessTokenParam getAccessTokenParam(Activity activity){
        if(ConvertUtils.isNull(activity)) return null;
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

    @Override
    protected void onBackPressed() {
        if (mBinding != null) {
            mBinding.scenePoiDetailContentView.onBackPressed();
        }
    }
}
