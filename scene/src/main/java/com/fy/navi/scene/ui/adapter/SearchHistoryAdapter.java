
package com.fy.navi.scene.ui.adapter;


import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SearchHistoryItemBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SearchHistoryAdapter extends RecyclerView.Adapter<SearchHistoryAdapter.ResultHolder> {
    private final List<History> mPoiEntities;
    private final SearchPackage mSearchPackage;
    private final BehaviorPackage mBehaviorPackage;
    private ItemClickListener mItemClickListener;
    private int mHomeCompanyType = -1;// 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是
    private boolean mShowActionContainer = true;
    private boolean mIsShowIndex = false;//显示icon还是序号,默认显示icon
    private History mHistory;

    public int getHomeCompanyType() {
        return mHomeCompanyType;
    }

    public void setHomeCompanyType(final int homeCompanyType) {
        this.mHomeCompanyType = homeCompanyType;
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
    public ResultHolder onCreateViewHolder(@NonNull final ViewGroup parent, final int viewType) {
        final SearchHistoryItemBinding adapterSearchResultItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.search_history_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull final ResultHolder holder, final int position) {
        holder.resultItemBinding.setPoiBean(mPoiEntities.get(position));
        mHistory = mPoiEntities.get(position);
        holder.resultItemBinding.setLayoutPosition(String.valueOf(position + 1));
        holder.resultItemBinding.sllCollect.setVisibility(View.VISIBLE);
        if (AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY == mPoiEntities.get(position).getMType()) {
            holder.resultItemBinding.skInfoLayout.setVisibility(View.GONE);
            holder.resultItemBinding.poiToNavi.setVisibility(View.GONE);
            holder.resultItemBinding.llActionContainer.setVisibility(mShowActionContainer ? View.VISIBLE : View.GONE);
            holder.resultItemBinding.sllCollect.setVisibility(View.INVISIBLE);
            holder.resultItemBinding.searchIcon.setVisibility(View.VISIBLE);
            holder.resultItemBinding.poiIcon.setVisibility(View.GONE);
            holder.resultItemBinding.searchIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.search_poi_icon));
        } else {
            if (mIsShowIndex) {
                holder.resultItemBinding.poiNum.setVisibility(View.VISIBLE);
                holder.resultItemBinding.poiIcon.setVisibility(View.GONE);
                holder.resultItemBinding.searchIcon.setVisibility(View.GONE);
            } else {
                holder.resultItemBinding.poiIcon.setVisibility(View.VISIBLE);
                holder.resultItemBinding.searchIcon.setVisibility(View.GONE);
            }
            holder.resultItemBinding.poiIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_orientation));
            holder.resultItemBinding.skInfoLayout.setVisibility(View.VISIBLE);
            holder.resultItemBinding.poiToNavi.setVisibility(View.VISIBLE);
            holder.resultItemBinding.llActionContainer.setVisibility(mShowActionContainer ? View.VISIBLE : View.GONE);
            if (!ConvertUtils.isEmpty(mPoiEntities.get(position).getMEndPoint())) {
                holder.resultItemBinding.poiDistance.setText(SearchPackage.getInstance().calcStraightDistance(
                        parseGeoPoint(mPoiEntities.get(position).getMEndPoint())));
            }
        }
        if (mPoiEntities.get(position) != null && ConvertUtils.isEmpty(mPoiEntities.get(position).getMEndPoiName())) {
            holder.resultItemBinding.subLineView.setVisibility(View.GONE);
        }
        if (mSearchPackage.isAlongWaySearch()) {
            holder.resultItemBinding.textNavi.setText(R.string.st_along_way_point);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_addq_58));

        } else {
            holder.resultItemBinding.textNavi.setText(R.string.st_go_here);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_navi));

        }

        if (mHomeCompanyType != -1) {
            // 1:家 2:公司 3:常用地址 0:收藏夹
            holder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
            switch (mHomeCompanyType) {
                case 3:
                    final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                    poiInfoEntity.setPid(mHistory.getMPoiId());
                    if (BehaviorPackage.getInstance().isFrequentAddress(poiInfoEntity)) {
                        holder.resultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                        holder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_route_search_added);
                    } else {
                        holder.resultItemBinding.textNavi.setText(R.string.st_collect_add);
                        holder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
                    }
                    break;
                case 0:
                    final PoiInfoEntity favInfo = new PoiInfoEntity();
                    favInfo.setName(mHistory.getMEndPoiName());
                    favInfo.setAddress(mHistory.getMEndPoiName());
                    favInfo.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                    favInfo.setPid(mHistory.getMPoiId());
                    final GeoPoint historyPoint = parseGeoPoint(mHistory.getMEndPoint());
                    final GeoPoint geoPoint = new GeoPoint();
                    geoPoint.setLon(historyPoint.getLon());
                    geoPoint.setLat(historyPoint.getLat());
                    favInfo.setPoint(geoPoint);
                    if (!BehaviorPackage.getInstance().isFavorite(favInfo).isEmpty()) {
                        holder.resultItemBinding.textNavi.setText(R.string.route_service_list_item_added);
                        holder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_route_search_added);
                    } else {
                        holder.resultItemBinding.textNavi.setText(R.string.st_collect_add);
                        holder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
                    }
                    break;
                case 1:
                    holder.resultItemBinding.textNavi.setText(R.string.st_home);
                    break;
                case 2:
                    holder.resultItemBinding.textNavi.setText(R.string.st_company);
                    break;
                default:
                    holder.resultItemBinding.textNavi.setText(R.string.st_go_here);
                    break;
            }
        }
        holder.resultItemBinding.crlPoiDes.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemClick(position, mPoiEntities.get(position));
            }
        });

        holder.resultItemBinding.poiToNavi.setOnClickListener(v -> {
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
                holder.resultItemBinding.ivCollect.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_star_filling58));
                holder.resultItemBinding.stvCollect.setText(R.string.sha_cancel);
            } else {
                holder.resultItemBinding.ivCollect.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_star58));
                holder.resultItemBinding.stvCollect.setText(R.string.sha_favorite);
            }
            holder.resultItemBinding.sllCollect.setOnClickListener(v -> {
                Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 收藏");
                if (isFavorite) {
                    //取消收藏： 仅当点击时，再从高德云端获取收藏数据，减少主线程刷新压力
                    final PoiInfoEntity favInfo = getFavoriteInfo(mPoiEntities.get(position));
                    mBehaviorPackage.removeFavorite(favInfo);
                    ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_cancel_favorite));
                } else {
                    addFavoriteInfo(mPoiEntities.get(position));
                    ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_has_favorite));
                }
                notifyDataSetChanged();
                holder.resultItemBinding.swipeMenuLayout.smoothClose();
            });
        }
        holder.resultItemBinding.sllDelete.setOnClickListener(v -> {
            holder.resultItemBinding.swipeMenuLayout.smoothClose();
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "poi click 删除");
            mSearchPackage.clearSearchKeywordRecord(mPoiEntities.get(position).getMId());
            if (position >= 0 && position < mPoiEntities.size()) {
                mPoiEntities.remove(position);
                notifyItemRemoved(position);
                if (mItemClickListener!= null) {
                    mItemClickListener.onDeleteClick(mPoiEntities);
                }
            }
            final List<History> historyList = mSearchPackage.getSearchKeywordRecord();
            ThreadManager.getInstance().postUi(() -> {
                notifyList(historyList);
            });
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_deleted));
        });
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
     * @param history
     */
    private void addFavoriteInfo(History history) {
        GeoPoint historyPoint = parseGeoPoint(history.getMEndPoint());
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                .setName(history.getMEndPoiName())
                .setAddress(history.getMEndPoiName())
                .setPid(history.getMPoiId())
                .setPoint(historyPoint);
        FavoriteInfo info = new FavoriteInfo().setCommonName(0);
        poiInfoEntity.setFavoriteInfo(info);
        mBehaviorPackage.addFavorite(poiInfoEntity, 0);
//        behaviorPackage.addFavoriteData(poiInfoEntity, 0);
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
        return mPoiEntities.size();
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

    public interface ItemClickListener {
        void onItemClick(int position, History poiInfoEntity);

        void onNaviClick(int position, History poiInfoEntity);
        default void onDeleteClick(List<History> poiInfoEntitys){}
    }

    public void setNoShowActionContainer(){
        mShowActionContainer = false;
    }
    public void setMIsShowIndex(final boolean isShowIndex){
        mIsShowIndex = isShowIndex;
    }
}