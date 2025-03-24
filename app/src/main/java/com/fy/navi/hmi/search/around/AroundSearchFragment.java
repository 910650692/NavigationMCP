package com.fy.navi.hmi.search.around;


import android.content.res.TypedArray;
import android.os.Bundle;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentAroundBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * 周边搜索页面
 */
@Route(path = RoutePath.Search.AROUND_SEARCH_FRAGMENT)
public class AroundSearchFragment extends BaseFragment<FragmentAroundBinding, AroundSearchViewModel> {
    @Override
    public int onLayoutId() {
        return R.layout.fragment_around;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.sceneQuickSearchView.setScreenId(MapTypeId.valueOf(mScreenId));

    }

    @Override
    public void onInitData() {
        defaultDataProcessing();
    }

    /**
     * 默认数据处理
     */
    private void defaultDataProcessing() {
        final Bundle parsedArgs = getArguments();
        if (parsedArgs == null) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "No valid arguments found.");
            return;
        }

        final PoiInfoEntity poiInfoEntity = parsedArgs.getParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_AROUND);
        mBinding.sceneQuickSearchView.setSearchType(AutoMapConstant.SearchType.AROUND_SEARCH);
        mBinding.sceneQuickSearchView.setPoiInfoEntity(poiInfoEntity);

        final String[] categories;
        final TypedArray iconArray;
        final String title;
        final int powerType = mViewModel.powerType();

        if (poiInfoEntity != null) {
            String poiName = poiInfoEntity.getName();
            if (poiName.length() > 4) {
                poiName = poiName.substring(0, 4) + "...";
            }
            categories = getResources().getStringArray(R.array.around_categories_name);
            iconArray = getResources().obtainTypedArray(R.array.around_category_icons);
            title = getString(R.string.around_search_title, poiName);
        } else {
            // 油车
            if (powerType == 0) {
                categories = getResources().getStringArray(R.array.main_search_more_categories_gas_name);
                iconArray = getResources().obtainTypedArray(R.array.main_search_more_category_gas_icons);
            } else {
                categories = getResources().getStringArray(R.array.main_search_more_categories_charging_name);
                iconArray = getResources().obtainTypedArray(R.array.main_search_more_category_charging_icons);
            }

            title = getString(R.string.around_search_title, getString(R.string.around_search_title_current_location));
        }

        mBinding.sceneQuickSearchView.setQuickSearchListAdapterData(iconArray, categories);
        mBinding.sceneQuickSearchView.setTextView(title);
    }


    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
