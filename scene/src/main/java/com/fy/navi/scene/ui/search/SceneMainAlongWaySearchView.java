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
import com.fy.navi.scene.api.search.IOnHomeCompanyClickListener;
import com.fy.navi.scene.databinding.SceneMainAlongWaySearchBarBinding;
import com.fy.navi.scene.impl.search.SceneMainAlongWaySearchViewImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.view.SkinTextView;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 沿途搜主页面 scene
 * @Date: 2020/4/16 11:08 AM
 * @CreateDate: $ $
 */
public class SceneMainAlongWaySearchView extends BaseSceneView<SceneMainAlongWaySearchBarBinding, SceneMainAlongWaySearchViewImpl> {
    private IOnHomeCompanyClickListener mClickListener;

    public SceneMainAlongWaySearchView(@NonNull final Context context) {
        super(context);
    }

    public SceneMainAlongWaySearchView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneMainAlongWaySearchView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public void setClickListener(final IOnHomeCompanyClickListener clickListener) {
        mClickListener = clickListener;
    }

    @Override
    protected SceneMainAlongWaySearchBarBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
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
        mViewBinding.ivGasStation.setImageResource(R.drawable.img_star58);
        mViewBinding.ivPullUp.setImageResource(R.drawable.img_broadcast_black_58);
        mViewBinding.ivGourmet.setImageResource(R.drawable.img_basic_ic_map_point_selection);
        mViewBinding.ivCarWashing.setImageResource(R.drawable.img_search58);
        mViewBinding.searchBarTextView.setHint(R.string.navi_add_via_hint);
        mViewBinding.searchBarTextView.setOnClickListener(v -> {
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.SUGGESTION_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createSugFragment(
                    AutoMapConstant.SourceFragment.FRAGMENT_MAIN_ALONG_WAY, AutoMapConstant.SearchType.ALONG_WAY_SEARCH));
        });
    }

    /**
     * 点击快速搜索
     *
     * @param position 点击下标
     */
    public void onClickQuickSearch(final int position) {
        // 收藏
        final Fragment fragment;
        switch (position) {
            case 0:
                fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.COLLECT_FRAGMENT)
                        .navigation();
                addFragment((BaseFragment) fragment, SearchFragmentFactory.createCollectFragment(
                        AutoMapConstant.SourceFragment.FRAGMENT_MAIN_ALONG_WAY,
                        AutoMapConstant.CollectionType.COLLECTION, AutoMapConstant.HomeCompanyType.COLLECTION));
                break;
            case 1:
                fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.COLLECT_FRAGMENT)
                        .navigation();
                if (fragment != null) {
                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createCollectFragment(
                                    AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY,
                                    AutoMapConstant.CollectionType.GET_POINT, AutoMapConstant.HomeCompanyType.ALONG_WAY));
                }
                break;
            case 2:
                //跳转地图选点页面，隐藏所有view
                if (null != mClickListener) {
                    mClickListener.setHomeCompanyType(AutoMapConstant.HomeCompanyType.ALONG_WAY);
                    mScreenViewModel.flyLineVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
                }
                break;
            case 3:
                fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.ALONG_WAY_SEARCH_FRAGMENT)
                        .navigation();
                addFragment((BaseFragment) fragment, null);
                break;
            default:
                break;
        }
    }

    /**
     * 设置 SkinTextView 的内容
     */
    private void setSkinTextViews() {
        if (mViewBinding != null) {
            final String[] categories = getResources().getStringArray(R.array.main_along_way_search_categories_name);
            final SkinTextView[] skinTextViews = {
                    mViewBinding.tvGasStation,
                    mViewBinding.tvPullUp,
                    mViewBinding.tvGourmet,
                    mViewBinding.tvCarWashing
            };
            for (int i = 0; i < skinTextViews.length && i < categories.length; i++) {
                skinTextViews[i].setText(categories[i]);
            }
        }
    }
}