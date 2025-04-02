package com.fy.navi.hmi.search.alongway;

import android.view.View;

import com.alibaba.android.arouter.facade.annotation.Route;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.MainFragmentAlongWayBinding;
import com.fy.navi.hmi.favorite.FavoriteHelper;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.api.search.IOnHomeCompanyClickListener;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
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
        mBinding.searchView.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNestedScrollView.setScreenId(MapType.valueOf(mScreenId));
        mBinding.searchView.setClickListener(new IOnHomeCompanyClickListener() {
            @Override
            public void onEditClearClicked() {
                //清除预搜索关键字后默认展示历史记录
                mBinding.sceneNestedScrollView.getSearchKeywordRecord();
            }

            @Override
            public void setHomeCompanyType(final int type) {
                FavoriteHelper.getInstance().setHomeCompanyType(type);
                mBinding.getRoot().setVisibility(View.GONE);
            }
        });
    }

    @Override
    public void onInitData() {
    }


    @Override
    public void onHiddenChanged(final boolean hidden) {
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
