package com.fy.navi.scene.ui.search;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SceneMainSearchBarBinding;
import com.fy.navi.scene.impl.search.SceneMainSearchViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索主页面 scene
 * @CreateDate: $ $
 */
public class SceneMainSearchTopPartView extends BaseSceneView<SceneMainSearchBarBinding, SceneMainSearchViewImpl> {
    private String[] mCategories;

    public SceneMainSearchTopPartView(@NonNull final Context context) {
        super(context);
    }

    public SceneMainSearchTopPartView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneMainSearchTopPartView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneMainSearchBarBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneMainSearchBarBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneMainSearchViewImpl initSceneImpl() {
        return new SceneMainSearchViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setMainSearchView(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        mViewBinding.searchBarTextView.setOnClickListener(v -> {
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SUGGESTION_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createSugFragment(
                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.SearchType.SEARCH_SUGGESTION));
        });
    }

    /**
     * 设置快捷点击事件
     * @param position 点击位置下标
     */
    public void onClickQuickSearch(final int position) {
        final int searchType = (position == 4) ? AutoMapConstant.SearchType.AROUND_SEARCH : AutoMapConstant.SearchType.SEARCH_KEYWORD;
        if (position == 4) {
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.AROUND_SEARCH_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createAroundFragment(null));
        } else {
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createKeywordFragment(
                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, searchType, mCategories[position], null));
        }
    }

    /**
     * 设置 SkinTextView 的内容
     * @param categories 类别数组
     * @param iconArray 图标数组
     */
    public void setSkinTextViews(final String[] categories, final TypedArray iconArray) {
        this.mCategories = categories;
        if (mViewBinding != null) {
            final SkinTextView[] skinTextViews = {
                    mViewBinding.tvGasStation,
                    mViewBinding.tvPullUp,
                    mViewBinding.tvGourmet,
                    mViewBinding.tvCarWashing,
                    mViewBinding.tvMore
            };
            for (int i = 0; i < skinTextViews.length && i < categories.length; i++) {
                skinTextViews[i].setText(categories[i]);
            }

            final SkinImageView[] skinImgViews = {
                    mViewBinding.ivGasStation,
                    mViewBinding.ivPullUp,
                    mViewBinding.ivGourmet,
                    mViewBinding.ivCarWashing,
                    mViewBinding.ivMore
            };
            for (int i = 0; i < skinImgViews.length && i < categories.length; i++) {
                final int resourceId = iconArray.getResourceId(i, 0);
                if (resourceId != 0) {
                    skinImgViews[i].setImageResource(resourceId);
                }
            }
        }
        iconArray.recycle();
    }
}