
package com.fy.navi.scene.ui.adapter;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

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
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.CollectResultItemBinding;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.ArrayList;
import java.util.List;

public class CollectResultAdapter extends RecyclerView.Adapter<CollectResultAdapter.ResultHolder> {
    private final SearchPackage searchPackage;
    private final List<PoiInfoEntity> poiEntities;
    private OnItemClickListener onItemClickListener;
    //0 普通收藏 1 常用地址 3 收到的点
    private int collectionType = 0;


    public void setOnItemClickListener(OnItemClickListener listener) {
        onItemClickListener = listener;
    }

    //0 普通收藏 1 常用地址 3 收到的点
    public void setCollectionType(int collectionType) {
        this.collectionType = collectionType;
        notifyList(poiEntities);
    }


    public CollectResultAdapter() {
        searchPackage = SearchPackage.getInstance();
        this.poiEntities = new ArrayList<>();
    }

    public void notifyList(List<PoiInfoEntity> searchResultEntity) {
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
        CollectResultItemBinding adapterSearchResultItemBinding = DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()), R.layout.collect_result_item, parent, false);
        return new ResultHolder(adapterSearchResultItemBinding);
    }

    @Override
    public void onBindViewHolder(@NonNull ResultHolder holder, int position) {
        holder.resultItemBinding.setPoiBean(poiEntities.get(position));
        holder.resultItemBinding.poiIcon.setVisibility(View.GONE);
        holder.resultItemBinding.poiNum.setVisibility(View.VISIBLE);
        holder.resultItemBinding.setLayoutPosition(String.valueOf(position + 1));
        if (!ConvertUtils.isEmpty(poiEntities.get(position).getFavoriteInfo())
                && poiEntities.get(position).getFavoriteInfo().getTop_time() != 0) {
            holder.resultItemBinding.itemFavoriteTopTag.setVisibility(View.VISIBLE);
            holder.resultItemBinding.itemFavoriteDistance.setVisibility(View.VISIBLE);
            holder.resultItemBinding.itemFavoriteLine.setVisibility(View.VISIBLE);
            holder.resultItemBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.bg_route_item_select));
            holder.resultItemBinding.stvTop.setText(ResourceUtils.Companion.getInstance().getString(R.string.dsc_cancel));
        } else {
            holder.resultItemBinding.itemFavoriteTopTag.setVisibility(View.GONE);
            holder.resultItemBinding.itemFavoriteDistance.setVisibility(View.GONE);
            holder.resultItemBinding.itemFavoriteLine.setVisibility(View.GONE);
            holder.resultItemBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
            holder.resultItemBinding.stvTop.setText(ResourceUtils.Companion.getInstance().getString(R.string.favorite_item_top));
        }
        if (searchPackage.isAlongWaySearch()) {
            holder.resultItemBinding.textNavi.setText(R.string.st_along_way_point);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_add_circle));

        } else {
            holder.resultItemBinding.textNavi.setText(R.string.st_go_here);
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_navi));
        }
//        Logger.d(SEARCH_HMI_TAG, "poiEntities.get(position) :"+poiEntities.get(position).getFavoriteInfo().getItemId());
        //若是常用地址或者收到的点，设置为加图标
        if (collectionType == AutoMapConstant.CollectionType.COMMON || collectionType == AutoMapConstant.CollectionType.GET_POINT) {
            holder.resultItemBinding.ivNaviIcon.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_basic_ic_add_circle));
            holder.resultItemBinding.textNavi.setText(R.string.mps_set_add);
            holder.resultItemBinding.llActionContainer.setVisibility(View.GONE); //隐藏左滑按钮
        }

        holder.resultItemBinding.crlPoiDes.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "poi click 详情");
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(position, poiEntities.get(position));
            }
        });

        holder.resultItemBinding.poiToNavi.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "poi click 算路/添加常用地址/添加收到的点");
            if (onItemClickListener != null) {
                onItemClickListener.onNaviClick(position, poiEntities.get(position));
            }
        });

        holder.resultItemBinding.sllTop.setOnClickListener(v -> {
            Logger.d(SEARCH_HMI_TAG, "poi click 置顶");
            if (poiEntities.get(position).getFavoriteInfo().getTop_time() != 0) {
                // 取消置顶
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                BehaviorPackage.getInstance().topFavorite(poiEntities.get(position), false);
                BehaviorPackage.getInstance().updateFavoriteTopTime(poiEntities.get(position).getFavoriteInfo().getItemId(), 0);
            } else {
                SettingUpdateObservable.getInstance().onUpdateSyncTime();
                BehaviorPackage.getInstance().topFavorite(poiEntities.get(position), true);
                long current = System.currentTimeMillis();
                BehaviorPackage.getInstance().updateFavoriteTopTime(poiEntities.get(position).getFavoriteInfo().getItemId(), current);
            }
            holder.resultItemBinding.swipeMenuLayout.smoothClose();
        });
        holder.resultItemBinding.sllDelete.setOnClickListener(v -> {
            PoiInfoEntity info = poiEntities.get(position);
            Logger.d(SEARCH_HMI_TAG, "poi click 删除");
            BehaviorPackage.getInstance().deleteFavoriteData(info.getFavoriteInfo().getItemId());
            if (position >= 0 && position < poiEntities.size()) {
                poiEntities.remove(position);
                notifyItemRemoved(position);
            }
            holder.resultItemBinding.swipeMenuLayout.smoothClose();
        });
    }

    @Override
    public int getItemCount() {
        return poiEntities.size();
    }

    public boolean isEmptyData() {
        return poiEntities.isEmpty();
    }

    public static class ResultHolder extends RecyclerView.ViewHolder {
        public CollectResultItemBinding resultItemBinding;

        public ResultHolder(CollectResultItemBinding resultItemBinding) {
            super(resultItemBinding.getRoot());
            this.resultItemBinding = resultItemBinding;
            this.resultItemBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int position, PoiInfoEntity poiInfoEntity);

        void onNaviClick(int position, PoiInfoEntity poiInfoEntity);
    }
}