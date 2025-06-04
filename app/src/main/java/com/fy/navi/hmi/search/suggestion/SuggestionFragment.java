package com.fy.navi.hmi.search.suggestion;


import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.SugSearchFragmentLayoutBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.api.search.IClearEditTextListener;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.ui.base.BaseFragment;

@Route(path = RoutePath.Search.SUGGESTION_FRAGMENT)
public class SuggestionFragment extends BaseFragment<SugSearchFragmentLayoutBinding, SuggestionResultViewModel> {

    @Override
    public int onLayoutId() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onLayoutId");
        return R.layout.sug_search_fragment_layout;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitView");
        mBinding.sceneSugPoiList.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onInitData");
        mBinding.sceneSugPoiList.setEditTextChangedListener(new IClearEditTextListener() {
            @Override
            public void onEditTextChanged(final String content) {
                mViewModel.onEditTextChanged(content);
            }
        });
    }

    @Override
    public void onGetFragmentData() {
        super.onGetFragmentData();
        getBundleData();
        mBinding.sceneSugPoiList.requestFocusAndShowKeyboard();
    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        mViewModel.onReStoreFragment();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        super.onHiddenChanged(hidden);
//        if (!hidden) {
//            mBinding.sceneSugPoiList.requestFocusAndShowKeyboard();
//        }
    }

    /**
     * 搜索结果回调
     * @param searchResultEntity 搜索结果实体类
     * @param isRestore 是否是切换日夜模式导致的更新回调
     */
    public void notifySearchResult(final SearchResultEntity searchResultEntity, final boolean isRestore) {
        if (searchResultEntity.getSearchType() == AutoMapConstant.SearchType.SEARCH_SUGGESTION) {
            mBinding.sceneSugPoiList.notifySearchResult(searchResultEntity, isRestore);
        }
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
//        int searchType = parsedArgs.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE);
//        String sourceFragmentTag = parsedArgs.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
    }
}
