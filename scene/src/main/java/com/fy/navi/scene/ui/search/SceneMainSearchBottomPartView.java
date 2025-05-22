package com.fy.navi.scene.ui.search;


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
import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneView;
import com.fy.navi.scene.R;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.databinding.SearchHistoryViewBinding;
import com.fy.navi.scene.impl.search.SceneSearchHistoryImpl;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.adapter.SearchHistoryAdapter;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
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
 * @author baipeng0904
 * @version \$Revision1.0\$
 * @Description: 搜索主页面历史记录 scene
 * @Date: 2020/4/16 11:05 AM
 * @CreateDate: $ $
 */
public class SceneMainSearchBottomPartView extends BaseSceneView<SearchHistoryViewBinding, SceneSearchHistoryImpl> {
    private SearchHistoryAdapter mSearchHistoryAdapter;

    public SceneMainSearchBottomPartView(@NonNull final Context context) {
        super(context);
    }

    public SceneMainSearchBottomPartView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);

    }

    public SceneMainSearchBottomPartView(@NonNull final Context context, @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);

    }

    @Override
    protected SearchHistoryViewBinding createViewBinding(final LayoutInflater inflater, final ViewGroup viewGroup) {
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
        final ArrayList<PoiInfoEntity> commonList = BehaviorPackage.getInstance().getFavoriteAddressInfo();
        final int count = Math.min(commonList.size(), 3);
        for (int i = 0; i < count; i++) {
            if (i == 0) {
                if (!ConvertUtils.isEmpty(commonList.get(i).getFavoriteInfo().getCustom_name())) {
                    setCommon(mViewBinding.searchHistoryMiddleView.ivAdd0, mViewBinding.searchHistoryMiddleView.tvAdd0,
                            commonList.get(i).getFavoriteInfo().getCustom_name());
                } else {
                    setCommon(mViewBinding.searchHistoryMiddleView.ivAdd0, mViewBinding.searchHistoryMiddleView.tvAdd0, commonList.get(i).getName());
                }
            } else if (i == 1) {
                if (!ConvertUtils.isEmpty(commonList.get(i).getFavoriteInfo().getCustom_name())) {
                    setCommon(mViewBinding.searchHistoryMiddleView.ivAdd1, mViewBinding.searchHistoryMiddleView.tvAdd1,
                            commonList.get(i).getFavoriteInfo().getCustom_name());
                } else {
                    setCommon(mViewBinding.searchHistoryMiddleView.ivAdd1, mViewBinding.searchHistoryMiddleView.tvAdd1, commonList.get(i).getName());
                }
            } else {
                if (!ConvertUtils.isEmpty(commonList.get(i).getFavoriteInfo().getCustom_name())) {
                    setCommon(mViewBinding.searchHistoryMiddleView.ivAdd2, mViewBinding.searchHistoryMiddleView.tvAdd2,
                            commonList.get(i).getFavoriteInfo().getCustom_name());
                } else {
                    setCommon(mViewBinding.searchHistoryMiddleView.ivAdd2, mViewBinding.searchHistoryMiddleView.tvAdd2, commonList.get(i).getName());
                }
            }
        }
    }

    /**
     *
     * @param iv ImageView引用
     * @param tv TextView引用
     * @param text 文本
     */
    private void setCommon(final ImageView iv, final TextView tv, final String text) {
        iv.setImageResource(R.drawable.img_basic_ic_orientation);
        tv.setText(text);
    }

    /**
     * 设置收藏按钮点击事件
     */
    private void setViewClick() {
        mViewBinding.searchHistoryMiddleView.sclStar.setOnClickListener(view -> {
            final Fragment fragment = (Fragment) ARouter.getInstance()
                    .build(RoutePath.Search.COLLECT_FRAGMENT)
                    .navigation();
            addFragment((BaseFragment) fragment, SearchFragmentFactory.createCollectFragment(
                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                    AutoMapConstant.CollectionType.COLLECTION, AutoMapConstant.HomeCompanyType.COLLECTION));

        });

        mViewBinding.sclDeleteRecord.setOnClickListener(view -> {
            new SearchConfirmDialog.Build(getContext())
                    .setDialogObserver(new IBaseDialogClickListener() {
                        @Override
                        public void onCommitClick() {
                            //清空历史记录
                            SearchPackage.getInstance().clearSearchKeywordRecord();
                            mSearchHistoryAdapter.notifyList(new ArrayList<>());
                            mViewBinding.rcyRecord.setVisibility(GONE);
                            mViewBinding.tvRecordNull.setVisibility(VISIBLE);
                            showDeleteRecord();
                            ToastUtils.Companion.getInstance().showCustomToastView(
                                    ResourceUtils.Companion.getInstance().getString(R.string.ssh_cleared_history_record));
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

    /**
     * 初始化适配器
     */
    private void initAdapter() {
        mSearchHistoryAdapter = new SearchHistoryAdapter();
        mViewBinding.rcyRecord.setAdapter(mSearchHistoryAdapter);
        mSearchHistoryAdapter.setOnItemClickListener(new SearchHistoryAdapter.ItemClickListener() {
            @Override
            public void onItemClick(final int position, final History history) {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "BP====onItemClick position:" + position + " history:" + history);
                if (history.getMType() == AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY) {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                            .navigation();

                    addFragment((BaseFragment) fragment,
                            SearchFragmentFactory.createKeywordFragment(
                                    AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                                    AutoMapConstant.SearchType.SEARCH_KEYWORD,
                                    history.getMKeyWord(),
                                    null));

                } else {
                    final GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setPid(history.getMPoiId())
                            .setPoint(historyPoint);
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                            AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_KEYWORD, poiInfoEntity));
                }
            }

            @Override
            public void onNaviClick(final int position, final History history) {
                final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                poiInfoEntity.setName(history.getMEndPoiName());
                poiInfoEntity.setAddress(history.getMEndPoiName());
                poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                poiInfoEntity.setPid(history.getMPoiId());
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onNaviClick: historyPoint = " + history.getMEndPoint());

                final GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
                final GeoPoint geoPoint = new GeoPoint();
                geoPoint.setLon(historyPoint.getLon());
                geoPoint.setLat(historyPoint.getLat());
                poiInfoEntity.setPoint(geoPoint);
                if (SearchPackage.getInstance().isAlongWaySearch()) {
                    RoutePackage.getInstance().addViaPoint(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity);
                } else {
                    final Fragment fragment = (Fragment) ARouter.getInstance()
                            .build(RoutePath.Route.ROUTE_FRAGMENT)
                            .navigation();
                    addFragment((BaseFragment) fragment, SearchFragmentFactory.createRouteFragment(poiInfoEntity));
                }
            }

            @Override
            public void onDeleteClick(List<History> poiInfoEntitys) {
                if (poiInfoEntitys == null || poiInfoEntitys.isEmpty()) {
                    ThreadManager.getInstance().postUi(() -> {
                        mViewBinding.tvRecordNull.setVisibility(VISIBLE);
                        mViewBinding.rcyRecord.setVisibility(GONE);
                        showDeleteRecord();
                    });
                }
            }
        });
    }

    /**
     * 初始化线性布局管理器
     */
    private void initLinearLayoutManager() {
        final LinearLayoutManager layoutManager = new LinearLayoutManager(getContext());
        layoutManager.setOrientation(LinearLayoutManager.VERTICAL);
        mViewBinding.rcyRecord.setLayoutManager(layoutManager);
    }

    /**
     * 通知关键词记录数据更新
     * @param historyList 历史记录
     */
    public void notifyKeywordRecord(final List<History> historyList) {
        if (!historyList.isEmpty()) {
            mSearchHistoryAdapter.notifyList(historyList);
            mSearchHistoryAdapter.setMIsShowIndex(false);
            mViewBinding.rcyRecord.setVisibility(VISIBLE);
            mViewBinding.tvRecordNull.setVisibility(GONE);
        } else {
//            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_no_data));
            mViewBinding.rcyRecord.setVisibility(GONE);
            mViewBinding.tvRecordNull.setVisibility(VISIBLE);
        }
        showDeleteRecord();
    }

    /**
     * 显示删除记录按钮
     */
    private void showDeleteRecord() {
        mViewBinding.sclDeleteRecord.setVisibility(mSearchHistoryAdapter.getPoiEntities().isEmpty() ? GONE : VISIBLE);
    }

    /**
     * 获取搜索关键词记录
     */
    public void getSearchKeywordRecord() {
        mScreenViewModel.getSearchKeywordRecord();
    }


    /**
     * 跳转到poi详情页
     * @param entity poi实体类
     */
    public void jumpToPoiFragment(final PoiInfoEntity entity) {
        final Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.POI_DETAILS_FRAGMENT)
                .navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createPoiDetailsFragment(
                AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT, AutoMapConstant.PoiType.POI_MAP_CLICK, entity));

    }

    /**
     * 跳转到家和公司主页
     */
    public void jumpToHomeCompanyFragment() {
        final Fragment fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.HOME_COMPANY_FRAGMENT)
                .navigation();
        addFragment((BaseFragment) fragment, SearchFragmentFactory.createHomeCompanyFragment(
                AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT,
                        AutoMapConstant.SearchType.SEARCH_KEYWORD,
                        AutoMapConstant.HomeCompanyType.COMMON));

    }

    /**
     * 解析geoPoint字符串
     * @param geoPointString geoPoint字符串
     * @return GeoPoint
     */
    private GeoPoint parseGeoPoint(final String geoPointString) {
        final Pattern pattern = Pattern.compile("lon=([-\\d.]+), lat=([-\\d.]+)");
        final Matcher matcher = pattern.matcher(geoPointString);

        double lon = 0.0;
        double lat = 0.0;

        if (matcher.find()) {
            lon = Double.parseDouble(matcher.group(1));
            lat = Double.parseDouble(matcher.group(2));
        } else {
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "parseGeoPoint: No match found for GeoPoint string: " + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }
}
