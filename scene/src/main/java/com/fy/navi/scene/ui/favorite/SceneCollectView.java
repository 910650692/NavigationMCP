package com.fy.navi.scene.ui.favorite;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

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
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SceneCollectViewBinding;
import com.fy.navi.scene.impl.favorite.SceneCollectViewImpl;
import com.fy.navi.scene.impl.search.FavoriteManager;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.CollectResultAdapter;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseFragment;

import java.util.Date;
import java.util.List;

/**
 * @Author: baipeng0904
 * @Description: 收藏夹
 * @CreateDate: $ $
 */
public class SceneCollectView extends BaseSceneView<SceneCollectViewBinding, SceneCollectViewImpl> {

    //0 普通收藏 1 常用地址 3 收到的点
    private int collectionType;
    private int homeCompanyType = -1;

    public SceneCollectView(@NonNull Context context) {
        super(context);
    }

    private CollectResultAdapter mAdapter;

    public SceneCollectView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneCollectView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneCollectViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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

    private void setupRecyclerView() {
        mViewBinding.rcvCollect.setLayoutManager(new LinearLayoutManager(getContext()));
        mAdapter = new CollectResultAdapter();
        mViewBinding.rcvCollect.setAdapter(mAdapter);
        mAdapter.setOnItemClickListener(new CollectResultAdapter.OnItemClickListener() {
            @Override
            public void onItemClick(int position, PoiInfoEntity poiInfoEntity) {

                Fragment fragment = (Fragment) ARouter.getInstance()
                        .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                        .navigation();
                if (collectionType == AutoMapConstant.CollectionType.COMMON || collectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    //常用地址/收到的点跳转进入 项目 应该是去详情页 按钮显示为添加
                    if (homeCompanyType == AutoMapConstant.HomeCompanyType.COMMON) {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_COMMON, poiInfoEntity));
                    } else {
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_COLLECTION, poiInfoEntity));
                    }
                } else {
                    // 正常状态下收藏夹进去 项目 应该是去算路 POI_COLLECTION
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.PoiType.POI_KEYWORD, poiInfoEntity));
                }
            }

            @Override
            public void onNaviClick(int position, PoiInfoEntity poiInfoEntity) {
                //如果是常用地址/收到的点跳转的收藏界面，那么点击导航按钮，根据来源页面的HomeCompany类型收藏为收藏点/常去的点
                if (collectionType == AutoMapConstant.CollectionType.COMMON || collectionType == AutoMapConstant.CollectionType.GET_POINT) {
                    Logger.d(SEARCH_HMI_TAG, "onNaviClick: " + collectionType + " ,homeCompanyType: " + homeCompanyType);
                    FavoriteManager.getInstance().addFavorite(poiInfoEntity, homeCompanyType);
                    closeAllFragment();
                } else {
                    if (SearchPackage.getInstance().isAlongWaySearch()) {
                        RoutePackage.getInstance().addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
                    } else {
                        Fragment fragment = (Fragment) ARouter.getInstance()
                                .build(RoutePath.Route.ROUTE_FRAGMENT)
                                .navigation();
                        addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                    }
                }
            }
        });
    }

    public void setViewClick() {
        mViewBinding.collectTitleBarView.ivClose.setOnClickListener(view -> mScreenViewModel.closeSearch());
        mViewBinding.collectTitleBarView.collectBarTextAdd.setOnClickListener(view -> {
            Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.HOME_COMPANY_FRAGMENT)
                    .navigation();
//            addFragment((BaseFragment) fragment, SearchFragmentFactory.createSugFragment(AutoMapConstant.SourceFragment.FRAGMENT_COLLECTION, AutoMapConstant.SearchType.SEARCH_SUGGESTION));
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createHomeCompanyFragment(AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.SearchType.SEARCH_KEYWORD, AutoMapConstant.HomeCompanyType.COLLECTION));
        });
    }

    public void setAdapterData(List<PoiInfoEntity> data) {
        mAdapter.notifyList(data);
    }

    public void setCollectionType(int collectionType) {
        Logger.d(SEARCH_HMI_TAG, "setCollectionType: " + collectionType);
        this.collectionType = collectionType;
        if(collectionType == AutoMapConstant.CollectionType.COMMON) {
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
        if(collectionType == AutoMapConstant.CollectionType.GET_POINT){
            mViewBinding.collectTitleBarView.collectBarTextView.setText(getResources().getString(R.string.shc_get_point));
            mViewBinding.collectTitleBarView.sclCollectAdd.setVisibility(View.GONE);
        }
        mAdapter.setCollectionType(collectionType);
    }

    public void setHomeCompanyType(int homeCompanyType) {
        this.homeCompanyType = homeCompanyType;
    }
}