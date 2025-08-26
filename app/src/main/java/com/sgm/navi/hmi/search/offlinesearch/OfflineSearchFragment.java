package com.sgm.navi.hmi.search.offlinesearch;


import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.OfflineSearchFragmentLayoutBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Search.OFFLINE_SEARCH_FRAGMENT)
public class OfflineSearchFragment extends BaseFragment<OfflineSearchFragmentLayoutBinding, OfflineSearchViewModel> {

    @Override
    public int onLayoutId() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.offline_search_fragment_layout;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitView");
        mBinding.offlineCityList.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitData");
        getBundleData();
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
        if (hidden) {
            mBinding.offlineCityList.hideInput();
        }
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 搜索结果实体类
     */
    public void notifySearchResult(final int taskId,final SearchResultEntity searchResultEntity) {
//        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            mBinding.offlineCityList.notifySearchResult(taskId,searchResultEntity);
//        }
    }

    /**
     * 获取bundle携带的数据
     */
    private void getBundleData() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        final String keyword = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD);
        mBinding.offlineCityList.setSearchText(keyword);
    }

    @Override
    protected void onBackPressed() {
        if (mBinding != null) {
            mBinding.offlineCityList.closeSearchFragment();
        }
    }
}
