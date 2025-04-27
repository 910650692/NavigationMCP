package com.fy.navi.scene.ui.favorite;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SceneCollectViewBinding;
import com.fy.navi.scene.impl.favorite.SceneCollectViewImpl;
import com.fy.navi.scene.impl.search.FavoriteManager;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.CollectResultAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.List;
import java.util.Objects;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 收藏夹
 * @CreateDate: $ $
 */
public class SceneCollectView extends BaseSceneView<SceneCollectViewBinding, SceneCollectViewImpl> {

    //0 普通收藏 1 常用地址 3 收到的点
    private int mCollectionType;
    private int mHomeCompanyType = -1;

    public SceneCollectView(@NonNull final Context context) {
        super(context);
    }

    private CollectResultAdapter mAdapter;

    public SceneCollectView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneCollectView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneCollectViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
        return SceneCollectViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneCollectViewImpl initSceneImpl() {
        return new SceneCollectViewImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneCollectViewImpl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        setupRecyclerView();
        setViewClick();
    }

    /**
     * 初始化RecyclerView
     */
    private void setupRecyclerView() {
        mViewBinding.rcvCollect.setLayoutManager(new LinearLayoutManager(getContext()));
        mAdapter = new CollectResultAdapter();
        mViewBinding.rcvCollect.setAdapter(mAdapter);
        mViewBinding.rcvCollect.setItemAnimator(null);
        mAdapter.setOnItemClickListener(new CollectResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(final int position, final PoiInfoEntity poiInfoEntity) {

                final Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                if (mCollectionType == AutoMapConstant.CollectionType.COMMON || mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    //常用地址/收到的点跳转进入 项目 应该是去详情页 按钮显示为添加
                    if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.COMMON) {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_COMMON, poiInfoEntity));
                    } else if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.ALONG_WAY) {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_AROUND, poiInfoEntity));
                    } else {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                                AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_COLLECTION, poiInfoEntity));
                    }
                } else {
                    // 正常状态下收藏夹进去 项目 应该是去算路 POI_COLLECTION
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                            AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_KEYWORD, poiInfoEntity));
                }
            }

            @Override
            public void onNaviClick(final int position, final PoiInfoEntity poiInfoEntity) {
                if (mCollectionType == AutoMapConstant.CollectionType.COMMON || mCollectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onNaviClick: " + mCollectionType + " ,homeCompanyType: " + mHomeCompanyType);
                    if (mHomeCompanyType == AutoMapConstant.HomeCompanyType.ALONG_WAY) {
                        //如果是添加途径点，则改为添加途径点后跳转到路线规划页面
                        if (SearchPackage.getInstance().isAlongWaySearch()) {
                            RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                        }
                        closeAllFragmentsUntilTargetFragment("MainAlongWaySearchFragment");
                    } else {
                        //如果是常用地址/收到的点跳转的收藏界面，那么点击导航按钮，根据来源页面的HomeCompany类型收藏为收藏点/常去的点
                        FavoriteManager.getInstance().addFavorite(poiInfoEntity, mHomeCompanyType);
                        closeAllFragmentsUntilTargetFragment("HomeCompanyFragment");
                        showCurrentFragment();
                    }
                } else {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                    } else {
                        final Fragment fragment = (Fragment) ARouter.getInstance()
                                .build(RoutePath.Route.ROUTE_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                    }
                }
            }

            @Override
            public void onListCleared() {
                mViewBinding.sllNoFavorite.setVisibility(View.VISIBLE);
            }
        });
    }

    /**
     * 设置点击事件
     */
    public void setViewClick() {
        mViewBinding.collectTitleBarView.ivClose.setOnClickListener(view -> mScreenViewModel.closeSearch());
        mViewBinding.collectTitleBarView.collectBarTextAdd.setOnClickListener(view -> {
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.HOME_COMPANY_FRAGMENT)
                    .navigation();
//            addFragment((BaseFragment) fragment, SearchFragmentFactory.createSugFragment(
//            AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.SearchType.SEARCH_SUGGESTION));
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createHomeCompanyFragment(
                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                    AutoMapConstant.SearchType.SEARCH_KEYWORD, AutoMapConstant.HomeCompanyType.COLLECTION));
        });
    }

    /**
     * 设置适配器数据
     * @param data data
     */
    public void setAdapterData(final List<PoiInfoEntity> data) {
        mAdapter.notifyList(data);
        if (mViewBinding != null) {
            if (ConvertUtils.isEmpty(data)) {
                mViewBinding.sllNoFavorite.setVisibility(View.VISIBLE);
            } else {
                mViewBinding.sllNoFavorite.setVisibility(View.GONE);
            }
        }
    }

    /**
     * 设置收藏类型
     * @param collectionType collectionType
     */
    public void setCollectionType(final int collectionType) {
        Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "setCollectionType: " + collectionType);
        this.mCollectionType = collectionType;
        if(collectionType == AutoMapConstant.CollectionType.COMMON) {
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
        if(collectionType == AutoMapConstant.CollectionType.GET_POINT){
            mViewBinding.collectTitleBarView.collectBarTextView.setText(getResources().getString(R.string.shc_get_point));
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
        mAdapter.setCollectionType(collectionType);
    }

    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
    }

    /**
     * 设置来源Fragment
     * @param sourceFragment sourceFragment
     */
    public void setSourceFragment(final String sourceFragment) {
        if (ConvertUtils.equals(sourceFragment, AutoMapConstant.SourceFragment.FRAGMENT_MAIN_ALONG_WAY)) {
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
    }
}