package com.fy.navi.hmi.favorite.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.databinding.DataBindingUtil;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.ResourceUtils;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ItemFavoriteBinding;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.List;

public class FavoriteDataAdapter extends RecyclerView.Adapter<FavoriteDataAdapter.Holder> {

    private static final String TAG = FavoriteDataAdapter.class.getSimpleName();
    private List<PoiInfoEntity> favoriteInfoList;
    private OnItemClickListener itemClickListener;

    public FavoriteDataAdapter() {
        this.favoriteInfoList = new ArrayList<>();
    }

    public void setData(List<PoiInfoEntity> list) {
        favoriteInfoList = list;
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        ItemFavoriteBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_favorite, parent, false);
        return new Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (favoriteInfoList == null) {
            return 0;
        }
        return favoriteInfoList.size();
    }

    @Override
    public void onBindViewHolder(@NonNull Holder holder, int position) {
        holder.favoriteBinding.setModel(favoriteInfoList.get(position));
        holder.favoriteBinding.setLayoutPosition(String.valueOf(position + 1));
        if (favoriteInfoList.get(position).getFavoriteInfo().getTop_time() != 0) {
            holder.favoriteBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.setting_bg_top));
            holder.favoriteBinding.itemFavoriteTopText.setText("取消");
            holder.favoriteBinding.itemFavoriteDistance.setVisibility(View.VISIBLE);
            holder.favoriteBinding.itemFavoriteLine.setVisibility(View.VISIBLE);
            holder.favoriteBinding.itemFavoriteTopTag.setVisibility(View.VISIBLE);
        } else {
            holder.favoriteBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
            holder.favoriteBinding.itemFavoriteTopText.setText("置顶");
            holder.favoriteBinding.itemFavoriteDistance.setVisibility(View.GONE);
            holder.favoriteBinding.itemFavoriteLine.setVisibility(View.GONE);
            holder.favoriteBinding.itemFavoriteTopTag.setVisibility(View.GONE);
        }
        //查看POI详情
        holder.favoriteBinding.contentLayout.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemDetailClick(position);
            }
        });

        // 导航到这里
        holder.favoriteBinding.itemFavoriteNavi.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemNaviClick(position);
            }
        });

        holder.favoriteBinding.itemFavoriteTop.setOnClickListener(v -> {

             if (favoriteInfoList.get(position).getFavoriteInfo().getTop_time() != 0) {
                if (itemClickListener != null) {
                    itemClickListener.onItemCancelTopClick(position);
                }
            } else if (itemClickListener != null) {
                itemClickListener.onItemTopClick(position);
            }
            holder.favoriteBinding.swipeMenuLayout.smoothClose();
        });

        holder.favoriteBinding.itemFavoriteDelete.setOnClickListener(v -> {
            holder.favoriteBinding.swipeMenuLayout.smoothClose();
            if (itemClickListener != null) {
                itemClickListener.onItemDeleteClick(position);
            }
        });

    }

    public static class Holder extends RecyclerView.ViewHolder {
        public ItemFavoriteBinding favoriteBinding;

        public Holder(ItemFavoriteBinding favoriteBinding) {
            super(favoriteBinding.getRoot());
            this.favoriteBinding = favoriteBinding;
            favoriteBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {
        void onItemDetailClick(int index);

        void onItemNaviClick(int index);

        void onItemTopClick(int index);

        void onItemCancelTopClick(int index);

        void onItemDeleteClick(int index);
    }
}
