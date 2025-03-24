
package com.fy.navi.scene.ui.adapter;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SearchHistoryItemBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class SearchHistoryAdapter extends RecyclerView.Adapter<SearchHistoryAdapter.ResultHolder> {
    private final List<History> poiEntities;
    private final SearchPackage searchPackage;
    private final BehaviorPackage behaviorPackage;
    private ItemClickListener itemClickListener;
    private int homeCompanyType = -1;// 1:家 2:公司 3:常用地址 0:收藏夹 -1:都不是

    public int getHomeCompanyType() {
        return homeCompanyType;
    }

    public void setHomeCompanyType(int homeCompanyType) {
        this.homeCompanyType = homeCompanyType;
    }
    public void setOnItemClickListener(ItemClickListener onItemClickListener) {
        this.itemClickListener = onItemClickListener;
    }

    public SearchHistoryAdapter() {
        searchPackage = SearchPackage.getInstance();
        behaviorPackage = BehaviorPackage.getInstance();
        this.poiEntities = new ArrayList<>();
    }

    public void notifyList(List<History> searchResultEntity) {
        int oldSize = poiEntities.size();
        int newSize = searchResultEntity.size();

        poiEntities.clear();
        poiEntities.addAll(searchResultEntity);

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
    public ResultHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        SearchHistoryItemBinding adapterSearchResultItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.search_history_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, int position) {
        holder.resultItemBinding.setPoiBean(poiEntities.get(position));
        holder.resultItemBinding.setLayoutPosition(String.valueOf(position + 1));
        if (AutoMapConstant.SearchKeywordRecordKey.SEARCH_KEYWORD_RECORD_KEY == poiEntities.get(position).getMType()) {
            holder.resultItemBinding.skInfoLayout.setVisibility(View.GONE);
            holder.resultItemBinding.poiToNavi.setVisibility(View.GONE);
            holder.resultItemBinding.llActionContainer.setVisibility(View.GONE);
        } else {
            holder.resultItemBinding.skInfoLayout.setVisibility(View.VISIBLE);
            holder.resultItemBinding.poiToNavi.setVisibility(View.VISIBLE);
            holder.resultItemBinding.llActionContainer.setVisibility(View.VISIBLE);
            holder.resultItemBinding.poiDistance.setText(SearchPackage.getInstance().calcStraightDistance(parseGeoPoint(poiEntities.get(position).getMEndPoint())));
        }

        if (searchPackage.isAlongWaySearch()) {
            holder.resultItemBinding.textNavi.setText(R.string.st_along_way_point);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_add));

        } else {
            holder.resultItemBinding.textNavi.setText(R.string.st_go_here);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_navi));

        }

        if (homeCompanyType != -1) {
            holder.resultItemBinding.ivNaviIcon.setImageResource(R.drawable.img_addq_58);
            switch (homeCompanyType) {
                case 0:
                case 3:
                    holder.resultItemBinding.textNavi.setText(R.string.st_collect_add);
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
            if (itemClickListener != null) {
                itemClickListener.onItemClick(position, poiEntities.get(position));
            }
        });

        holder.resultItemBinding.poiToNavi.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onNaviClick(position, poiEntities.get(position));
            }
        });
        PoiInfoEntity poiInfo = getFavoriteInfo(poiEntities.get(position));
        boolean isFavorite = poiInfo != null;
        if (isFavorite) {
            holder.resultItemBinding.ivCollect.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_star));
            holder.resultItemBinding.stvCollect.setText(R.string.sha_cancel);
        } else {
            holder.resultItemBinding.ivCollect.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.icon_basic_ic_star_default));
            holder.resultItemBinding.stvCollect.setText(R.string.sha_favorite);
        }
        holder.resultItemBinding.sllCollect.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "poi click 收藏");
            if (isFavorite) {
                // 取消收藏
                behaviorPackage.deleteFavoriteData(poiInfo.getFavoriteInfo().getItemId());
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_cancel_favorite));
            } else {
                addFavoriteInfo(poiEntities.get(position));
                ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_has_favorite));
            }
            notifyItemChanged(position);
            holder.resultItemBinding.swipeMenuLayout.smoothClose();
        });
        holder.resultItemBinding.sllDelete.setOnClickListener(v -> {
            holder.resultItemBinding.swipeMenuLayout.smoothClose();
            Logger.d(SEARCH_HMI_TAG, "poi click 删除");
            searchPackage.clearSearchKeywordRecord(poiEntities.get(position).getMId());
            if (position >= 0 && position < poiEntities.size()) {
                poiEntities.remove(position);
                notifyItemRemoved(position);
            }
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.sha_deleted));
        });
    }

    /**
     * 获取收藏点信息
     *
     * @param history
     */
    private PoiInfoEntity getFavoriteInfo(History history) {
        List<PoiInfoEntity> list = behaviorPackage.getFavoritePoiData(0);
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
        FavoriteInfo info = new FavoriteInfo().setCommonName(0)
                .setItemId(poiInfoEntity.getPid() + "_" + poiInfoEntity.getName() + "_" + poiInfoEntity.getPoint().getLon() + "_" + poiInfoEntity.getPoint().getLat());
        poiInfoEntity.setFavoriteInfo(info);
        behaviorPackage.addFavoriteData(poiInfoEntity, 0);
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


    @Override
    public int getItemCount() {
        return poiEntities.size();
    }

    public List<History> getPoiEntities() {
        return poiEntities;
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
    }
}