package com.fy.navi.scene.ui.search;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SceneMainAlongWaySearchBarBinding;
import com.fy.navi.scene.impl.search.SceneMainAlongWaySearchViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.SkinTextView;

/**
 * @Author: baipeng0904
 * @Description: 沿途搜主页面 scene
 * @Date: 2020/4/16 11:08 AM
 * @CreateDate: $ $
 */
public class SceneMainAlongWaySearchView extends BaseSceneView<SceneMainAlongWaySearchBarBinding, SceneMainAlongWaySearchViewImpl> {
    public SceneMainAlongWaySearchView(@NonNull Context context) {
        super(context);
    }

    public SceneMainAlongWaySearchView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneMainAlongWaySearchView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneMainAlongWaySearchBarBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneMainAlongWaySearchBarBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneMainAlongWaySearchViewImpl initSceneImpl() {
        return new SceneMainAlongWaySearchViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setMainSearchView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setSkinTextViews();
        mViewBinding.searchBarTextView.setOnClickListener(v -> {
            Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SUGGESTION_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createSugFragment(AutoMapConstant.SourceFragment.FRAGMENT_MAIN_ALONG_WAY, AutoMapConstant.SearchType.ALONG_WAY_SEARCH));
        });
    }

    public void onClickQuickSearch(int position) {
        // 收藏
        if (position == 0) {
            Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.COLLECT_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createCollectFragment(AutoMapConstant.SourceFragment.FRAGMENT_MAIN_ALONG_WAY, AutoMapConstant.CollectionType.COLLECTION, AutoMapConstant.HomeCompanyType.COLLECTION));
            // 沿途类型搜索
        } else if (position == 3) {
            Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.ALONG_WAY_SEARCH_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, null);

        }
    }

    /**
     * 设置 SkinTextView 的内容
     */
    private void setSkinTextViews() {
        if (mViewBinding != null) {
            String[] categories = getResources().getStringArray(R.array.main_along_way_search_categories_name);
            SkinTextView[] skinTextViews = {
                    mViewBinding.tvGasStation,
                    mViewBinding.tvPullUp,
                    mViewBinding.tvGourmet,
                    mViewBinding.tvCarWashing,
            };
            for (int i = 0; i < skinTextViews.length && i < categories.length; i++) {
                skinTextViews[i].setText(categories[i]);
            }
        }
    }
}