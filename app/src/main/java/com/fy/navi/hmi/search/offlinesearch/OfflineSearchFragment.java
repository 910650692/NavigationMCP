package com.fy.navi.hmi.search.offlinesearch;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.OfflineSearchFragmentLayoutBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Search.OFFLINE_SEARCH_FRAGMENT)
public class OfflineSearchFragment extends BaseFragment<OfflineSearchFragmentLayoutBinding, OfflineSearchViewModel> {

    @Override
    public int onLayoutId() {
        Logger.d(SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.offline_search_fragment_layout;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(SEARCH_HMI_TAG, "onInitView");
        mBinding.offlineCityList.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        Logger.d(SEARCH_HMI_TAG, "onInitData");
        getBundleData();
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
//        if (!hidden) {
//            mBinding.offlineCityList.requestFocusAndShowKeyboard();
//        }
    }

    public void notifySearchResult(SearchResultEntity searchResultEntity) {
//        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            mBinding.offlineCityList.notifySearchResult(searchResultEntity);
//        }
    }

    private void getBundleData() {
        Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }
        String keyword = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD);
        mBinding.offlineCityList.setSearchText(keyword);
    }
}
