
package com.sgm.navi.scene.ui.adapter;


import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SearchHistoryFooterBinding;
import com.sgm.navi.scene.databinding.SearchHistoryItemBinding;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.usertrack.HistoryRouteItemBean;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SearchHistoryAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    private static final int TYPE_ITEM = 0;
    private static final int TYPE_FOOTER = 1;
    private final List<History> mPoiEntities;
    private final SearchPackage mSearchPackage;
    private final BehaviorPackage mBehaviorPackage;
    private ItemClickListener mItemClickListener;
    private int mHomeCompanyType = -1;// 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
    private boolean mShowActionContainer = true;
    private boolean mIsShowIndex = false;//显示icon还是序号,默认显示icon
    private History mHistory;
    private boolean mIsHasFooter = false;
    //历史记录列表界面是否仅显示历史导航记录。目前沿途搜界面仅需要展示历史导航记录
    private boolean mIsOnlyShowNaviRecord = false;

    public void setMIsOnlyShowNaviRecord(final boolean isOnlyShowNaviRecord) {
        this.mIsOnlyShowNaviRecord = isOnlyShowNaviRecord;
    }



    public int getHomeCompanyType() {
        return mHomeCompanyType;
    }

    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
        notifyDataSetChanged();
    }
    public void setOnItemClickListener(final ItemClickListener onItemClickListener) {
        this.mItemClickListener = onItemClickListener;
    }

    public SearchHistoryAdapter() {
        mSearchPackage = SearchPackage.getInstance();
        mBehaviorPackage = BehaviorPackage.getInstance();
        mShowActionContainer = true;
        this.mPoiEntities = new ArrayList<>();
    }


    public boolean isHasFooter() {
        return mIsHasFooter;
    }

    public void setMIsHasFooter(final boolean isHasFooter) {
        this.mIsHasFooter = isHasFooter;
    }


    @Override
    public int getItemViewType(int position) {
        if (position == mPoiEntities.size()) {
            return TYPE_FOOTER;
        }
        return TYPE_ITEM;
    }

    /**
     * 更新列表
     * @param searchResultEntity 源数据
     */
    public void notifyList(final List<History> searchResultEntity) {
        final int oldSize = mPoiEntities.size();
        final int newSize = searchResultEntity.size();

        mPoiEntities.clear();
        mPoiEntities.addAll(searchResultEntity);
        if (oldSize == 0 && newSize > 0) {
            notifyItemRangeInserted(0, newSize);
        } else if (oldSize > 0 && newSize == 0) {
            notifyItemRangeRemoved(0, oldSize);
        } else if (oldSize > 0) {
            notifyItemRangeChanged(0, Math.min(oldSize, newSize));
            if (newSize > oldSize) {
                notifyItemRangeInserted(oldSize, newSize - oldSize);
            } else if (newSize < oldSize) {
                notifyItemRangeRemoved(newSize, oldSize - newSize);
            }
        }
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        if (viewType == TYPE_ITEM) {
            final SearchHistoryItemBinding adapterSearchResultItemBinding
                    = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.search_history_item, parent, false);
            return new ResultHolder(adapterSearchResultItemBinding);
        } else {
            final SearchHistoryFooterBinding historyFooterBinding
                    = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.search_history_footer, parent, false);
            return new FooterViewHolder(historyFooterBinding);
        }
    }

    @Override
    public void onBindViewHolder(@NonNull final RecyclerView.ViewHolder holder, final int position) {
        if (holder instanceof ResultHolder resultHolder) {
            resultHolder.resultItemBinding.setPoiBean(mPoiEntities.get(position));
            mHistory = mPoiEntities.get(position);
            resultHolder.resultItemBinding.setLayoutPosition(String.valueOf(position + 1));
            resultHolder.resultItemBinding.sllCollect.setVisibility(View.VISIBLE);
            if (AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY == mPoiEntities.get(position).getMType()) {
                resultHolder.resultItemBinding.skInfoLayout.setVisibility(View.GONE);
                resultHolder.resultItemBinding.poiToNavi.setVisibility(View.GONE);
                resultHolder.resultItemBinding.llActionContainer.setVisibility(mShowActionContainer ? View.VISIBLE : View.GONE);
                resultHolder.resultItemBinding.sllCollect.setVisibility(View.INVISIBLE);
                resultHolder.resultItemBinding.searchIcon.setVisibility(View.VISIBLE);
                resultHolder.resultItemBinding.poiIcon.setVisibility(View.GONE);
                resultHolder.resultItemBinding.stvDelete.setVisibility(View.VISIBLE);
                resultHolder.resultItemBinding.searchIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.search_poi_icon));
            } else {
                if (mIsShowIndex) {
                    resultHolder.resultItemBinding.poiNum.setVisibility(View.VISIBLE);
                    resultHolder.resultItemBinding.poiIcon.setVisibility(View.GONE);
                    resultHolder.resultItemBinding.searchIcon.setVisibility(View.GONE);
                } else {
                    resultHolder.resultItemBinding.poiIcon.setVisibility(View.VISIBLE);
                    resultHolder.resultItemBinding.searchIcon.setVisibility(View.GONE);
                }
                resultHolder.resultItemBinding.poiIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_orientation));
                resultHolder.resultItemBinding.skInfoLayout.setVisibility(View.VISIBLE);
                resultHolder.resultItemBinding.poiToNavi.setVisibility(View.VISIBLE);
                resultHolder.resultItemBinding.stvDelete.setVisibility(View.VISIBLE);
                resultHolder.resultItemBinding.llActionContainer.setVisibility(mShowActionContainer ? View.VISIBLE : View.GONE);
                if (!ConvertUtils.isEmpty(mPoiEntities.get(position).getMEndPoint())) {
                    resultHolder.resultItemBinding.poiDistance.setText(SearchPackage.getInstance().calcStraightDistance(
                            parseGeoPoint(mPoiEntities.get(position).getMEndPoint())));
                }
            }
            if (mPoiEntities.get(position) != null && ConvertUtils.isEmpty(mPoiEntities.get(position).getMEndPoiName())) {
                resultHolder.resultItemBinding.subLineView.setVisibility(View.GONE);
            }
            if (mSearchPackage.isAlongWaySearch()) {
                PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                        .setPid(mHistory.getMPoiId())
                        .setName(mHistory.getMEndPoiName())
                        .setAddress(mHistory.getMAddress());
                if (!ConvertUtils.isEmpty(mPoiEntities.get(position).getMEndPoint())) {
                    poiInfoEntity.setPoint(parseGeoPoint(mPoiEntities.get(position).getMEndPoint()));
                }
                if (RoutePackage.getInstance().isBelongRouteParam(MapType.MAIN_SCREEN_MAIN_MAP, poiInfoEntity)) {
                    resultHolder.resultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                    resultHolder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance()
                            .getDrawable(R.drawable.img_route_search_added));
                } else {
                    resultHolder.resultItemBinding.textNavi.setText(R.string.st_along_way_point);
                    resultHolder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_addq_58));
                }

            } else {
                resultHolder.resultItemBinding.textNavi.setText(R.string.st_go_here);
                resultHolder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_navi));

            }

            if (mHomeCompanyType != -1) {
                // 1:家 2:公司 3:常用地址 0:收藏夹
                resultHolder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
                switch (mHomeCompanyType) {
                    case 3:
                        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                        poiInfoEntity.setPid(mHistory.getMPoiId());
                        if (BehaviorPackage.getInstance().isFrequentAddress(poiInfoEntity)) {
                            resultHolder.resultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                            resultHolder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_route_search_added);
                        } else {
                            resultHolder.resultItemBinding.textNavi.setText(R.string.st_collect_add);
                            resultHolder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
                        }
                        break;
                    case 0:
                        final PoiInfoEntity favInfo = new PoiInfoEntity();
                        favInfo.setName(mHistory.getMEndPoiName());
                        favInfo.setAddress(mHistory.getMAddress());
                        favInfo.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                        favInfo.setPid(mHistory.getMPoiId());
                        final GeoPoint historyPoint = parseGeoPoint(mHistory.getMEndPoint());
                        final GeoPoint geoPoint = new GeoPoint();
                        geoPoint.setLon(historyPoint.getLon());
                        geoPoint.setLat(historyPoint.getLat());
                        favInfo.setPoint(geoPoint);
                        if (!BehaviorPackage.getInstance().isFavorite(favInfo).isEmpty()) {
                            resultHolder.resultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                            resultHolder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_route_search_added);
                        } else {
                            resultHolder.resultItemBinding.textNavi.setText(R.string.st_collect_add);
                            resultHolder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
                        }
                        break;
                    case 1:
                        resultHolder.resultItemBinding.textNavi.setText(R.string.st_home);
                        break;
                    case 2:
                        resultHolder.resultItemBinding.textNavi.setText(R.string.st_company);
                        break;
                    default:
                        resultHolder.resultItemBinding.textNavi.setText(R.string.st_go_here);
                        break;
                }
            }
            resultHolder.resultItemBinding.crlPoiDes.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    mItemClickListener.onItemClick(position, mPoiEntities.get(position));
                }
            });

            resultHolder.resultItemBinding.poiToNavi.setOnClickListener(v -> {
                if (mItemClickListener != null) {
                    mItemClickListener.onNaviClick(position, mPoiEntities.get(position));
                }
            });
            if (AutoMapConstant.SearchKeywordRecordKey.SEARCH_NAVI_RECORD_KEY == mPoiEntities.get(position).getMType()) {
                final PoiInfoEntity poiInfo = new PoiInfoEntity().setPid(mPoiEntities.get(position).getMPoiId());
                if (!ConvertUtils.isEmpty(mPoiEntities.get(position).getMEndPoint())) {
                    final GeoPoint historyPoint = parseGeoPoint(mPoiEntities.get(position).getMEndPoint());
                    poiInfo.setPoint(historyPoint);
                }
                poiInfo.setName(mPoiEntities.get(position).getMEndPoiName());
                final boolean isFavorite = !ConvertUtils.isEmpty(mBehaviorPackage.isFavorite(poiInfo));
                if (isFavorite) {
                    resultHolder.resultItemBinding.ivCollect.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_star_filling58));
                    resultHolder.resultItemBinding.stvCollect.setText(R.string.sha_cancel);
                } else {
                    resultHolder.resultItemBinding.ivCollect.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_star58));
                    resultHolder.resultItemBinding.stvCollect.setText(R.string.sha_favorite);
                }
                resultHolder.resultItemBinding.sllCollect.setOnClickListener(v -> {
                    Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 收藏");
                    if (isFavorite) {
                        //取消收藏： 仅当点击时，再从高德云端获取收藏数据，减少主线程刷新压力
                        final PoiInfoEntity favInfo = getFavoriteInfo(mPoiEntities.get(position));
                        mBehaviorPackage.removeFavorite(favInfo);
                        ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.
                                Companion.getInstance().getString(R.string.sha_cancel_favorite));
                    } else {
                        addFavoriteInfo(mPoiEntities.get(position));
                        ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.
                                Companion.getInstance().getString(R.string.sha_has_favorite));
                    }
                    notifyItemChanged(position);
//                    notifyDataSetChanged();
                    resultHolder.resultItemBinding.swipeMenuLayout.quickClose();
                });
            }
            resultHolder.resultItemBinding.sllDelete.setOnClickListener(v -> {
                resultHolder.resultItemBinding.swipeMenuLayout.smoothClose();
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 删除");
                if (AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY == mPoiEntities.get(position).getMType()) {
                    UserTrackPackage.getInstance().delSearchHistory(mPoiEntities.get(position).getMKeyWord());
                } else {
                    final HistoryRouteItemBean itemBean = new HistoryRouteItemBean();
                    if (mSearchPackage.isLogin()) {
                        itemBean.setId(mPoiEntities.get(position).getMNaviHistoryId());
                        itemBean.setStartLoc(parseGeoPoint(mPoiEntities.get(position).getMStartPoint()));
                        itemBean.setEndLoc(parseGeoPoint(mPoiEntities.get(position).getMEndPoint()));
                        itemBean.setIsCompleted(mPoiEntities.get(position).getMIsCompleted());
                        itemBean.setUpdateTime(mPoiEntities.get(position).getMUpdateTime().getTime());
                    } else {
                        itemBean.setId(mPoiEntities.get(position).getMId().toString());
                    }
                    UserTrackPackage.getInstance().delHistoryRoute(itemBean);
//                mSearchPackage.clearSearchKeywordRecord(mPoiEntities.get(position).getMId());
                }
                if (position >= 0 && position < mPoiEntities.size()) {
                    mPoiEntities.remove(position);
                    notifyItemRemoved(position);
                    if (mItemClickListener!= null) {
                        mItemClickListener.onDeleteClick(mPoiEntities);
                    }
                }
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "mIsOnlyShowNaviRecord: " + mIsOnlyShowNaviRecord);
                final List<History> historyList;
                if (mIsOnlyShowNaviRecord) {
                    historyList = mSearchPackage.getNaviRecord();
                } else {
                    historyList = mSearchPackage.getSearchKeywordRecord();
                }
                ThreadManager.getInstance().postUi(() -> {
                    notifyList(historyList);
                });
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_deleted));
            });
        } else if (holder instanceof FooterViewHolder){
            ((FooterViewHolder) holder).historyFooterBinding.getRoot().setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(final View view) {
                    if (mItemClickListener!= null) {
                        mItemClickListener.onDeleteAllClick();
                    }
                }
            });
        }

    }

    /**
     * 获取收藏点信息
     *
     * @param history
     */
    private PoiInfoEntity getFavoriteInfo(History history) {
        List<PoiInfoEntity> list = mBehaviorPackage.getFavoritePoiData();
        return list.stream()
                .filter(item -> item.getPid().equals(history.getMPoiId()))
                .findFirst().orElse(null);
    }

    /**
     * 添加收藏
     *
     * @param history 历史记录
     */
    private void addFavoriteInfo(final History history) {
        final GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                .setName(history.getMEndPoiName())
                .setAddress(history.getMAddress())
                .setPid(history.getMPoiId())
                .setPoint(historyPoint);
        final FavoriteInfo info = new FavoriteInfo().setCommonName(0);
        poiInfoEntity.setFavoriteInfo(info);
        mBehaviorPackage.addFavorite(poiInfoEntity, 0);
//        behaviorPackage.addFavoriteData(poiInfoEntity, 0);
    }

    /**
     * 判断是否登录
     * @return 是否登录
     */
    private boolean isLogin() {
        final AccountProfileInfo info;
        final String valueJson = CommonManager.getInstance().getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        if (!TextUtils.isEmpty(valueJson)) {
            info = GsonUtils.fromJson(valueJson, AccountProfileInfo.class);
            if (info != null) {
                return !TextUtils.isEmpty(info.getUid());
            }
        }
        return false;
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
            Logger.e(MapDefaultFinalTag.SEARCH_HMI_TAG, "parseGeoPoint: No match found for GeoPoint string: " + geoPointString);
        }
        return new GeoPoint(lon, lat);
    }


    @Override
    public int getItemCount() {
        if (mIsHasFooter) {
            return mPoiEntities.size() + 1;
        } else {
            return mPoiEntities.size();
        }
    }

    public List<History> getPoiEntities() {
        return mPoiEntities;
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public SearchHistoryItemBinding resultItemBinding;

        public ResultHolder(SearchHistoryItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.resultItemBinding = resultItemBinding;
            this.resultItemBinding.setHolder(this);
        }
    }

    // 定义Footer的ViewHolder
    public static class FooterViewHolder extends RecyclerView.ViewHolder {
        public SearchHistoryFooterBinding historyFooterBinding;
        public FooterViewHolder(SearchHistoryFooterBinding historyFooterBinding) {
            super(historyFooterBinding.getRoot());
            this.historyFooterBinding = historyFooterBinding;
            this.historyFooterBinding.setHolder(this);
        }
    }

    public interface ItemClickListener {
        void onItemClick(int position, History poiInfoEntity);

        void onNaviClick(int position, History poiInfoEntity);
        default void onDeleteClick(List<History> poiInfoEntitys){}

        default void onDeleteAllClick(){}

    }

    public void setNoShowActionContainer(){
        mShowActionContainer = false;
    }
    public void setMIsShowIndex(final boolean isShowIndex){
        mIsShowIndex = isShowIndex;
    }
}