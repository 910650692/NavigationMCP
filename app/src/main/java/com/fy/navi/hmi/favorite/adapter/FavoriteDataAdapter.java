package com.fy.navi.hmi.favorite.adapter;

import android.annotation.SuppressLint;
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
    private List<PoiInfoEntity> mFavoriteInfoList;
    private OnItemClickListener mItemClickListener;

    public FavoriteDataAdapter() {
        this.mFavoriteInfoList = new ArrayList<>();
    }

    /**
     * setData
     * @param list list
     */
    @SuppressLint("NotifyDataSetChanged")
    public void setData(final List<PoiInfoEntity> list) {
        mFavoriteInfoList = list;
        notifyDataSetChanged();
    }

    public void setItemClickListener(final OnItemClickListener itemClickListener) {
        this.mItemClickListener = itemClickListener;
    }

    @NonNull
    @Override
    public Holder onCreateViewHolder(final @NonNull ViewGroup parent,  final int viewType) {
        final ItemFavoriteBinding routeItemBinding =
                DataBindingUtil.inflate(LayoutInflater.from(parent.getContext()),
                        R.layout.item_favorite, parent, false);
        return new Holder(routeItemBinding);
    }

    @Override
    public int getItemCount() {
        if (mFavoriteInfoList == null) {
            return 0;
        }
        return mFavoriteInfoList.size();
    }

    @Override
    public void onBindViewHolder(final @NonNull Holder holder, final int position) {
        holder.mFavoriteBinding.setModel(mFavoriteInfoList.get(position));
        holder.mFavoriteBinding.setLayoutPosition(String.valueOf(position + 1));
        if (mFavoriteInfoList.get(position).getFavoriteInfo().getTop_time() != 0) {
            holder.mFavoriteBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.setting_bg_top));
            holder.mFavoriteBinding.itemFavoriteTopText.setText("取消");
            holder.mFavoriteBinding.itemFavoriteDistance.setVisibility(View.VISIBLE);
            holder.mFavoriteBinding.itemFavoriteLine.setVisibility(View.VISIBLE);
            holder.mFavoriteBinding.itemFavoriteTopTag.setVisibility(View.VISIBLE);
        } else {
            holder.mFavoriteBinding.swipeMenuLayout.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.color.transparent));
            holder.mFavoriteBinding.itemFavoriteTopText.setText("置顶");
            holder.mFavoriteBinding.itemFavoriteDistance.setVisibility(View.GONE);
            holder.mFavoriteBinding.itemFavoriteLine.setVisibility(View.GONE);
            holder.mFavoriteBinding.itemFavoriteTopTag.setVisibility(View.GONE);
        }
        //查看POI详情
        holder.mFavoriteBinding.contentLayout.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemDetailClick(position);
            }
        });

        // 导航到这里
        holder.mFavoriteBinding.itemFavoriteNavi.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                mItemClickListener.onItemNaviClick(position);
            }
        });

        holder.mFavoriteBinding.itemFavoriteTop.setOnClickListener(v -> {
            if (mItemClickListener != null) {
                if (mFavoriteInfoList.get(position).getFavoriteInfo().getTop_time() != 0) {
                    mItemClickListener.onItemCancelTopClick(position);
                } else {
                    mItemClickListener.onItemTopClick(position);
                }
            }
            holder.mFavoriteBinding.swipeMenuLayout.smoothClose();
        });

        holder.mFavoriteBinding.itemFavoriteDelete.setOnClickListener(v -> {
            holder.mFavoriteBinding.swipeMenuLayout.smoothClose();
            if (mItemClickListener != null) {
                mItemClickListener.onItemDeleteClick(position);
            }
        });

    }

    public static class Holder extends RecyclerView.ViewHolder {
        private ItemFavoriteBinding mFavoriteBinding;

        public Holder(final ItemFavoriteBinding favoriteBinding) {
            super(favoriteBinding.getRoot());
            this.mFavoriteBinding = favoriteBinding;
            mFavoriteBinding.setHolder(this);
        }
    }

    public interface OnItemClickListener {

        /**
         * onItemDetailClick
         * @param index
         */
        void onItemDetailClick(int index);

        /**
         * onItemNaviClick
         * @param index
         */
        void onItemNaviClick(int index);

        /**
         * onItemTopClick
         * @param index
         */
        void onItemTopClick(int index);

        /**
         * onItemCancelTopClick
         * @param index
         */
        void onItemCancelTopClick(int index);

        /**
         * onItemDeleteClick
         * @param index
         */
        void onItemDeleteClick(int index);
    }
}
