package com.fy.navi.scene.ui.search;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

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
import com.fy.navi.scene.databinding.SearchHistoryViewBinding;
import com.fy.navi.scene.impl.search.SceneSearchHistoryImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.SearchHistoryAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * @Author: baipeng0904
 * @Description: 搜索主页面历史记录 scene
 * @Date: 2020/4/16 11:05 AM
 * @CreateDate: $ $
 */
public class SceneMainSearchBottomPartView extends BaseSceneView<SearchHistoryViewBinding, SceneSearchHistoryImpl> {
    private SearchHistoryAdapter searchHistoryAdapter;

    public SceneMainSearchBottomPartView(@NonNull Context context) {
        super(context);
    }

    public SceneMainSearchBottomPartView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);

    }

    public SceneMainSearchBottomPartView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);

    }

    @Override
    protected SearchHistoryViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SearchHistoryViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneSearchHistoryImpl initSceneImpl() {
        return new SceneSearchHistoryImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSceneSearchHistoryImpl(mScreenViewModel);
        mViewBinding.searchHistoryMiddleView.setSceneSearchHistoryImpl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        initLinearLayoutManager();
        initAdapter();
        setViewClick();
        getSearchKeywordRecord();
        initRefreshCommonAddress();
    }

    /**
     * 获取常用地址信息
     */
    public void initRefreshCommonAddress() {
        ArrayList<PoiInfoEntity> commonList = BehaviorPackage.getInstance().getFavoritePoiData(3);
        int count = Math.min(commonList.size(), 3);
        for (int i = 0; i < count; i++) {
            if (i == 0)
                setCommon(mViewBinding.searchHistoryMiddleView.ivAdd0, mViewBinding.searchHistoryMiddleView.tvAdd0, commonList.get(i).getName());
            if (i == 1)
                setCommon(mViewBinding.searchHistoryMiddleView.ivAdd1, mViewBinding.searchHistoryMiddleView.tvAdd1, commonList.get(i).getName());
            if (i == 2)
                setCommon(mViewBinding.searchHistoryMiddleView.ivAdd2, mViewBinding.searchHistoryMiddleView.tvAdd2, commonList.get(i).getName());
        }
    }

    private void setCommon(ImageView iv, TextView tv, String text) {
        iv.setImageResource(R.drawable.img_basic_ic_orientation);
        tv.setText(text);
    }

    private void setViewClick() {
        // 设置收藏按钮点击事件
        mViewBinding.searchHistoryMiddleView.sclStar.setOnClickListener(view -> {
            Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.COLLECT_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createCollectFragment(AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.CollectionType.COLLECTION, AutoMapConstant.HomeCompanyType.COLLECTION));

        });

        mViewBinding.sclDeleteRecord.setOnClickListener(view -> {
            new SearchConfirmDialog.Build(getContext())
                    .setDialogObserver(new IBaseDialogClickListener() {
                        @Override
                        public void onCommitClick() {
                            //清空历史记录
                            SearchPackage.getInstance().clearSearchKeywordRecord();
                            searchHistoryAdapter.notifyList(new ArrayList<>());
                            showDeleteRecord();
                            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.ssh_cleared_history_record));
                        }

                        @Override
                        public void onCancelClick() {

                        }
                    })
                    .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.dialog_title_tip))
                    .setContent(ResourceUtils.Companion.getInstance().getString(R.string.dialog_content_clear_record))
                    .setConfirmTitle(ResourceUtils.Companion.getInstance().getString(R.string.dsc_confirm))
                    .build().show();
        });
    }

    private void initAdapter() {
        searchHistoryAdapter = new SearchHistoryAdapter();
        mViewBinding.rcyRecord.setAdapter(searchHistoryAdapter);
        searchHistoryAdapter.setOnItemClickListener(new SearchHistoryAdapter.ItemClickListener() {
            @Override
            public void onItemClick(int position, History history) {
                Logger.d(SEARCH_HMI_TAG, "BP====onItemClick position:" + position + " history:" + history);
                if (history.type == AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY) {
                    Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                            .navigation();

                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createKeywordFragment(
                                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                                    AutoMapConstant.SearchType.SEARCH_KEYWORD,
                                    history.keyWord,
                                    null));

                } else {
                    GeoPoint historyPoint = parseGeoPoint(history.endPoint);
                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setPid(history.poiId)
                            .setPoint(historyPoint);
                    Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_KEYWORD, poiInfoEntity));
                }
            }

            @Override
            public void onNaviClick(int position, History history) {
                PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                poiInfoEntity.setName(history.endPoiName);
                poiInfoEntity.setAddress(history.endPoiName);
                poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                poiInfoEntity.setPid(history.poiId);
                Logger.d(SEARCH_HMI_TAG, "onNaviClick: historyPoint = " + history.endPoint);

                GeoPoint historyPoint = parseGeoPoint(history.endPoint);
                GeoPoint geoPoint = new GeoPoint();
                geoPoint.setLon(historyPoint.getLon());
                geoPoint.setLat(historyPoint.getLat());
                poiInfoEntity.setPoint(geoPoint);
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    RoutePackage.getInstance().addViaPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP, poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
                } else {
                    Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Route.ROUTE_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                }
            }
        });
    }

    private void initLinearLayoutManager() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.rcyRecord.setLayoutManager(layoutManager);
    }

    public void notifyKeywordRecord(List<History> historyList) {
        if (historyList != null) {
            searchHistoryAdapter.notifyList(historyList);
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_no_data));
        }
        showDeleteRecord();
    }

    private void showDeleteRecord() {
        mViewBinding.sclDeleteRecord.setVisibility(searchHistoryAdapter.getPoiEntities().isEmpty() ? GONE : VISIBLE);
    }

    public void getSearchKeywordRecord() {
        mScreenViewModel.getSearchKeywordRecord();
    }


    public void jumpToPoiFragment(PoiInfoEntity entity) {
        Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                .navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_MAP_CLICK, entity));

    }

    public void jumpToHomeCompanyFragment() {
        Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.HOME_COMPANY_FRAGMENT)
                .navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createHomeCompanyFragment(AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.SearchType.SEARCH_KEYWORD, AutoMapConstant.HomeCompanyType.COMMON));

    }

    private GeoPoint parseGeoPoint(String geoPointString) {
        Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        Matcher matcher = pattern.matcher(geoPointString);

        double lon = 0.0;
        double lat = 0.0;

        if (matcher.find()) {
            lon = Double.parseDouble(matcher.group(1));
            lat = Double.parseDouble(matcher.group(2));
        } else {
            Logger.e(SEARCH_HMI_TAG, "parseGeoPoint: No match found for GeoPoint string: " + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }
}
