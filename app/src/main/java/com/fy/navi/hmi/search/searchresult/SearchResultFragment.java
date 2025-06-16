package com.fy.navi.hmi.search.searchresult;


import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSearchResultBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Search.SEARCH_RESULT_FRAGMENT)
public class SearchResultFragment extends BaseFragment<FragmentSearchResultBinding, SearchResultViewModel> {
    private String mSourceFragmentTag;
    private boolean mIsEnd;
    private int mTaskId;
    private SearchResultEntity mSearchResultEntity;
    private int mHomeCompany;

    @Override
    public int onLayoutId() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.fragment_search_result;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitView");
        mBinding.scenePoiList.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitData");
    }

    @Override
    public void onGetFragmentData() {
        super.onGetFragmentData();
        //正常进入Fragment请求数据并刷新界面
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onGetFragmentData " + mBinding.scenePoiList);
        getBundleData();
        
    }

    @Override
    public void onReStoreFragment() {
        //日夜模式切换使用保存的数据恢复界面
        super.onReStoreFragment();
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onReStoreFragment ");
        mViewModel.onReStoreFragment();
    }

    /**
     * 获取bundle数据
     */
    private void getBundleData() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        mSourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
        final int searchType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE);
        final String keyword = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD);
        final PoiInfoEntity entity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST);
        final int range= parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_RANGE, 5000);
        final int isOpenFromNavi = parsedArgs.getInt(NaviConstant.NAVI_CONTROL, 0);
        final int cityCode = parsedArgs.getInt("cityCode", 0);
        mIsEnd = parsedArgs.getBoolean("IS_END" , false);//是否是语音设置终点，如果是的话，即使在导航态也需要显示去这里
        if (isOpenFromNavi == 1) {
            mBinding.scenePoiList.setNaviControl(true);
        }
        mBinding.scenePoiList.setPoiInfoEntity(entity);
        mBinding.scenePoiList.setRange(range);
        mBinding.scenePoiList.setCityCode(cityCode);
        mBinding.scenePoiList.setEditText(searchType, keyword);
        if (searchType == AutoMapConstant.SearchType.ALONG_WAY_SEARCH) {
            mViewModel.registerRouteCallback();
        }
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "sourceFragmentTag : " + mSourceFragmentTag);
    }

    /**
     * 搜索结果回调
     * @param taskId 请求回调id
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mHomeCompany = -1;
        if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_HOME)) {
            mHomeCompany = AutoMapConstant.HomeCompanyType.HOME;
        } else if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COMPANY)) {
            mHomeCompany = AutoMapConstant.HomeCompanyType.COMPANY;
        } else if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION)) {
            mHomeCompany = AutoMapConstant.HomeCompanyType.COLLECTION;
        } else if (ConvertUtils.equals(mSourceFragmentTag, AutoMapConstant
                .SourceFragment.FRAGMENT_COMMON)) {
            mHomeCompany = AutoMapConstant.HomeCompanyType.COMMON;
        }
        mTaskId = taskId;
        mSearchResultEntity = searchResultEntity;
        mBinding.scenePoiList.setHomeCompanyState(mHomeCompany);
        mBinding.scenePoiList.setMIsEnd(mIsEnd);
        mBinding.scenePoiList.notifySearchResult(mTaskId, mSearchResultEntity);
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "notifySearchResult " + mSearchResultEntity);

    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            updateShowState(true);
            mBinding.scenePoiList.reloadPoiMarker(getLastClosedFragmentName());
        } else {
            updateShowState(false);
        }
    }

    /**
     * 静默搜索回调
     * @param searchResultEntity 搜索回调实体类
     * @param taskId 任务id
     */
    public void notifySilentSearchResult(final int taskId, final SearchResultEntity searchResultEntity) {
        mBinding.scenePoiList.notifySilentSearchResult(taskId, searchResultEntity);
    }

    /**
     * 路线变化回调
     */
    public void onRouteSelected() {
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                closeAllFragmentUpRoute();
            }
        });
    }

    /**
     * 语音筛选搜索回调
     * @param sortValue 筛选条件
     */
    public void onVoicePoiSort(final String sortValue) {
        mBinding.scenePoiList.onVoicePoiSort(sortValue);
    }

    /**
     * 图层点击事件回调
     * @param index 点击下标
     */
    public void onMarkClickCallBack(final int index) {
        mBinding.scenePoiList.onMarkClickCallBack(index);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        updateShowState(false);
        mBinding.scenePoiList.clear();
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();
        mViewModel.saveData(mBinding.scenePoiList.getTaskId(), mBinding.scenePoiList.getResultEntity());
    }

    public void notifySearchResultByNet(final int taskId, final SearchResultEntity searchResultEntity){
        mBinding.scenePoiList.notifySearchResult(taskId, searchResultEntity);
    }

    public void notifySearchResultByNetError(int taskId,String message){
        mBinding.scenePoiList.notifySearchResultNetError(taskId,message);
    }

    @Override
    public void onResume() {
        super.onResume();
        updateShowState(true);
    }

    @Override
    public void onPause() {
        super.onPause();
        updateShowState(false);
    }

    /**
     * 搜索结果列表页面可变状态变化回调
     * @param isShow 是否可见
     */
    private void updateShowState(final boolean isShow) {
        mViewModel.updateShowState(isShow);
    }
}
