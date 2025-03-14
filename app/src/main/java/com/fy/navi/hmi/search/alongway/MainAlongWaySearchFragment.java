package com.fy.navi.hmi.search.alongway;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.MainFragmentAlongWayBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.ui.base.BaseFragment;

/**
 * 沿途搜索页面
 */
@Route(path = RoutePath.Search.MAIN_ALONG_WAY_SEARCH_FRAGMENT)
public class MainAlongWaySearchFragment extends BaseFragment<MainFragmentAlongWayBinding, MainAlongWaySearchViewModel> {

    @Override
    public int onLayoutId() {
        return R.layout.main_fragment_along_way;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        mBinding.searchView.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setScreenId(MapTypeId.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
    }


    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            mBinding.sceneNestedScrollView.getSearchKeywordRecord();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }
}
