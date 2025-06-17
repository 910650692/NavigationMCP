package com.fy.navi.hmi.mapdata.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;

import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.utils.StringUtils;
import com.fy.navi.scene.ui.setting.DownloadBtnView;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.mapdata.CityDataInfo;
import com.fy.navi.service.define.mapdata.CityDownLoadInfo;
import com.fy.navi.service.define.mapdata.ProvDataInfo;
import com.fy.navi.ui.view.CustomSwipeMenuLayout;
import com.fy.navi.ui.view.SkinButton;
import com.fy.navi.ui.view.SkinImageView;
import com.fy.navi.ui.view.SkinTextView;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class MapDataAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {
    private static final int TYPE_PARENT = 0;
    private static final int TYPE_CHILD = 1;
    private Context mContext;
    private List<ProvDataInfo> parentItems = new ArrayList<>();
    private OnChildClickListener onChildClickListener;

    public MapDataAdapter(final Context context) {
        this.mContext = context;
    }

    public void setData(List<ProvDataInfo> parentItems) {
        this.parentItems = parentItems;
        notifyDataSetChanged();
    }

    public void setOnChildClickListener(final OnChildClickListener listener) {
        this.onChildClickListener = listener;
    }

    @Override
    public int getItemViewType(int position) {
        int itemCount = 0;

        for (ProvDataInfo parent : parentItems) {
            if (position == itemCount) {
                return TYPE_PARENT;
            }
            itemCount++;

            if (parent.isExpanded()) {
                if (position < itemCount + parent.getCityInfoList().size()) {
                    return TYPE_CHILD;
                }
                itemCount += parent.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        LayoutInflater inflater = LayoutInflater.from(parent.getContext());
        if (viewType == TYPE_PARENT) {
            View view = inflater.inflate(R.layout.item_province, parent, false);
            return new ParentViewHolder(view);
        } else {
            View view = inflater.inflate(R.layout.item_city_data, parent, false);
            return new ChildViewHolder(view);
        }
    }

    @Override
    public void onBindViewHolder(RecyclerView.ViewHolder holder, int position) {
        int itemCount = 0;

        for (ProvDataInfo parent : parentItems) {
            if (position == itemCount) {
                ((ParentViewHolder) holder).bind(parent);
                return;
            }
            itemCount++;

            if (parent.isExpanded()) {
                if (position < itemCount + parent.getCityInfoList().size()) {
                    int childPos = position - itemCount;
                    ((ChildViewHolder) holder).bind(parent, parent.getCityInfoList().get(childPos));
                    return;
                }
                itemCount += parent.getCityInfoList().size();
            }
        }
        throw new IllegalStateException("Invalid position");
    }

    @Override
    public int getItemCount() {
        if (parentItems != null && !parentItems.isEmpty()) {
            int count = parentItems.size();
            for (ProvDataInfo parent : parentItems) {
                if (parent.isExpanded()) {
                    count += parent.getCityInfoList().size();
                }
            }
            return count;
        } else {
            return 0;
        }
    }

    /**
     * 父项ViewHolder
     */
    class ParentViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView title;
        private SkinImageView expandIcon;

        ParentViewHolder(View itemView) {
            super(itemView);
            title = itemView.findViewById(R.id.item_province_name);
            expandIcon = itemView.findViewById(R.id.item_province_expand);

            itemView.setOnClickListener(v -> {
                // 使用绝对位置方法（兼容新旧版本）
                int position = getAbsoluteAdapterPosition();
                if (position != RecyclerView.NO_POSITION) {
                    // 获取对应的父项数据
                    ProvDataInfo parent = parentItems.get(getParentPosition(position));
                    // 记录之前的展开状态
                    boolean wasExpanded = parent.isExpanded();
                    // 切换展开状态
                    parent.setExpanded(!wasExpanded);
                    // 局部刷新当前父项（只更新这一项）
                    notifyItemChanged(position);
                    // 计算子项数量
                    int childCount = parent.getCityInfoList().size();
                    if (wasExpanded) {
                        // 如果之前是展开状态，现在要折叠 - 移除子项
                        notifyItemRangeRemoved(position + 1, childCount);
                    } else {
                        // 如果之前是折叠状态，现在要展开 - 添加子项
                        notifyItemRangeInserted(position + 1, childCount);
                    }
                }
            });
        }

        void bind(ProvDataInfo parent) {
            title.setText(parent.getName()); //省份名称
            expandIcon.setImageResource(
                    parent.isExpanded() ? R.drawable.img_up_58 : R.drawable.img_under_the_58
            );
        }
    }

    /**
     * 子项ViewHolder
     */
    class ChildViewHolder extends RecyclerView.ViewHolder {
        private SkinTextView cityName;
        private SkinTextView cityData;
        private DownloadBtnView mDownloadBtnView;
        private CustomSwipeMenuLayout swipeMenuLayout;
        private SkinButton allDownload;
        private SkinButton allPause;
        private SkinTextView deleteCity;
        private ProgressBar downloadProgress;

        ChildViewHolder(final View itemView) {
            super(itemView);
            cityName = itemView.findViewById(R.id.item_city_name);
            cityData = itemView.findViewById(R.id.item_city_data);
            swipeMenuLayout = itemView.findViewById(R.id.swipe_menu_layout);
            mDownloadBtnView = itemView.findViewById(R.id.item_download_status);
            allDownload = itemView.findViewById(R.id.item_all_download);
            allPause = itemView.findViewById(R.id.item_all_pause);
            deleteCity = itemView.findViewById(R.id.item_driving_delete);
            downloadProgress = itemView.findViewById(R.id.download_progress);
        }

        void bind(final ProvDataInfo parent, final CityDataInfo child) {

            final CityDownLoadInfo downloadItem = child.getDownLoadInfo();
            if (downloadItem == null) {
                return;
            }
            // 非已下载状态，禁止侧滑删除
            if (downloadItem.getTaskState() == UserDataCode.TASK_STATUS_CODE_SUCCESS) {
                swipeMenuLayout.setSwipeEnabled(true);
            } else {
                swipeMenuLayout.setSwipeEnabled(false);
                swipeMenuLayout.smoothClose();
            }
            // 城市名称
            cityName.setText(child.getName());
            // 城市数据包大小
            final String sizeString = StringUtils.formatSize(downloadItem.getFullZipSize().longValue());
            cityData.setText(sizeString);

            if (child.getName().equals("全省下载")) {
                mDownloadBtnView.setVisibility(View.GONE);
                allDownload.setVisibility(View.VISIBLE);
                allPause.setVisibility(View.VISIBLE);
                downloadProgress.setVisibility(View.GONE);
                final Map<Integer, Integer> allCityTaskStateMap = downloadItem.getAllCityTaskStateMap();
                boolean isAllDownloadEnable = false;
                boolean isAllPauseEnable = false;
                if (allCityTaskStateMap != null) {
                    if (allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_PAUSE)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_ERR)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_READY)) {
                        isAllDownloadEnable = true;
                    }

                    if (allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_DOING)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_DONE)
                        || allCityTaskStateMap.containsValue(UserDataCode.TASK_STATUS_CODE_WAITING)) {
                        isAllPauseEnable = true;
                    }
                }
                allDownload.setEnabled(isAllDownloadEnable);
                allPause.setEnabled(isAllPauseEnable);
            } else {
                mDownloadBtnView.setVisibility(View.VISIBLE);
                allDownload.setVisibility(View.GONE);
                allPause.setVisibility(View.GONE);
                //下载按钮状态
                mDownloadBtnView.parseDownloadStatusInfo(child.getDownLoadInfo());
                boolean isShowDownloadProgress = false;
                if (child.getDownLoadInfo() != null) {
                    isShowDownloadProgress = child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING
                            || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE
                            || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_UNZIPPING
                            || child.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE;
                } else {
                    Logger.d("child.getDownLoadInfo() == null: " + GsonUtils.toJson(parent));
                    Logger.d("child.getDownLoadInfo() == null: " + GsonUtils.toJson(child));
                }
                if (isShowDownloadProgress) {
                    downloadProgress.setProgress((int) Math.floor(child.getDownLoadInfo().getPercent()));
                    downloadProgress.setVisibility(View.VISIBLE);
                } else {
                    downloadProgress.setVisibility(View.GONE);
                }
            }

            // 下载 or 暂停下载
            mDownloadBtnView.setOnClickListener(v -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (onChildClickListener != null && downloadItem != null) {
                    switch (downloadItem.getTaskState()) {
                        case UserDataCode.TASK_STATUS_CODE_DOING:  // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_DONE:   // 下载中 or 更新中（downloadItem.bIsDataUsed = true 更新中）
                        case UserDataCode.TASK_STATUS_CODE_WAITING: // 等待中 or 等待更新中
                            onChildClickListener.pauseAllTask(cityAdCodes);
                            break;
                        case UserDataCode.TASK_STATUS_CODE_PAUSE:  // 暂停
                        case UserDataCode.TASK_STATUS_CODE_READY:  // 待下载 or 待更新 (downloadItem.bIsDataUsed = true 待更新)
                        case UserDataCode.TASK_STATUS_CODE_ERR: // 错误 - 重试
                        case UserDataCode.TASK_STATUS_CODE_MAX: // 重试
                            onChildClickListener.startAllTask(cityAdCodes);
                            break;
                        default:
                            break;
                    }
                }
            });
            // 删除已下载数据
            deleteCity.setOnClickListener(view -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                cityAdCodes.add(downloadItem.getAdcode());
                if (onChildClickListener != null) {
                    onChildClickListener.deleteAllTask(cityAdCodes);
                }
            });
            // 全部开始下载
            allDownload.setOnClickListener(view -> {
                if (parent == null) {
                    Logger.d("parent: parent is null");
                    return;
                }
                Logger.d("parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final List<CityDataInfo> cityDataInfos = parent.getCityInfoList();
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                if (cityDataInfos != null && cityDataInfos.size() != 0) {
                    for (CityDataInfo info : cityDataInfos) {
                        if (info != null && info.getDownLoadInfo() != null) {
                            if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_PAUSE ||
                                    info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_READY ||
                                    info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_ERR ||
                                    info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_MAX) {
                                cityAdCodes.add(info.getAdcode());
                            }
                        } else {
                            Logger.d("info == null: " + GsonUtils.toJson(info));
                        }
                    }
                }
                if (onChildClickListener != null) {
                    onChildClickListener.allDownloadTask(cityAdCodes);
                }
            });
            // 全部暂停下载
            allPause.setOnClickListener(view -> {
                Logger.d( "parent: " + GsonUtils.toJson(parent) + " child: " + GsonUtils.toJson(child));
                final List<CityDataInfo> cityDataInfos = parent.getCityInfoList();
                final ArrayList<Integer> cityAdCodes = new ArrayList<>();
                if (cityDataInfos != null && cityDataInfos.size() != 0) {
                    for (CityDataInfo info : cityDataInfos) {
                        if (info != null && info.getDownLoadInfo() != null) {
                            if (info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DOING ||
                                    info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_DONE ||
                                    info.getDownLoadInfo().getTaskState() == UserDataCode.TASK_STATUS_CODE_WAITING) {
                                cityAdCodes.add(info.getAdcode());
                            }
                        } else {
                            Logger.d("info == null: " + GsonUtils.toJson(info));
                        }
                    }
                }
                if (onChildClickListener != null) {
                    onChildClickListener.allPauseTask(cityAdCodes);
                }
            });
        }
    }

    /**
     * 更新子项数据
     * @param parentId
     * @param childId
     * @param newValue
     */
    public void updateChild(final int parentId, final int childId, final CityDownLoadInfo newValue) {
        final int childPosition = getChildPosition(parentId, childId);
        //获取全省下载条目位置
        final int allDownloadPosition = getChildPosition(parentId, parentId);
        for (ProvDataInfo parent : parentItems) {
            if (parent.getAdcode() == parentId) {
                for (CityDataInfo child : parent.getCityInfoList()) {
                    //全省下载条目
                    if (child.getAdcode() == parentId) {
                        final CityDownLoadInfo downLoadInfo = child.getDownLoadInfo();
                        final Map<Integer, Integer> allCityTaskStateMap = downLoadInfo.getAllCityTaskStateMap();
                        if (allCityTaskStateMap != null) {
                            allCityTaskStateMap.put(childId, newValue.getTaskState());
                            if (allDownloadPosition != RecyclerView.NO_POSITION) {
                                notifyItemChanged(allDownloadPosition);
                            }
                        }
                    }
                    if (child.getAdcode() == childId ) {
                        child.setDownLoadInfo(newValue);
                        if (childPosition != RecyclerView.NO_POSITION) {
                            notifyItemChanged(childPosition);
                        }
                        return;
                    }
                }
            }
        }
    }

    /**
     * 获取父项在原始列表中的位置（不考虑子项）
     * @param adapterPosition
     * @return parent position
     */
    private int getParentPosition(final int adapterPosition) {
        int parentPos = 0;
        int currentPos = 0;

        for (ProvDataInfo parent : parentItems) {
            if (adapterPosition == currentPos) {
                return parentPos;
            }
            currentPos++;

            if (parent.isExpanded()) {
                if (adapterPosition <= currentPos + parent.getCityInfoList().size() - 1) {
                    return parentPos;
                }
                currentPos += parent.getCityInfoList().size();
            }

            parentPos++;
        }
        return -1;
    }

    /**
     * 新增方法：根据父项和子项ID获取子项的绝对位置
     * @param parentId
     * @param childId
     * @return child position
     */
    private int getChildPosition(final int parentId, final int childId) {
        int position = 0;
        for (ProvDataInfo parent : parentItems) {
            position++; // 父项位置

            if (parent.getAdcode() == parentId) {
                if (parent.isExpanded()) {
                    for (CityDataInfo child : parent.getCityInfoList()) {
                        if (child.getAdcode() == childId) {
                            return position;
                        }
                        position++;
                    }
                }
                break;
            }

            if (parent.isExpanded()) {
                position += parent.getCityInfoList().size();
            }
        }
        return RecyclerView.NO_POSITION; // 未找到
    }

    public interface OnChildClickListener {

        /**
         * 开始下载
         * @param cityAdCodes
         */
        void startAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 暂停下载
         * @param cityAdCodes
         */
        void pauseAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 删除
         * @param cityAdCodes
         */
        void deleteAllTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 取消下载
         */
        void cancelAllTask();

        /**
         * 全部下载
         * @param cityAdCodes
         */
        void allDownloadTask(final ArrayList<Integer> cityAdCodes);

        /**
         * 全部暂停
         * @param cityAdCodes
         */
        void allPauseTask(final ArrayList<Integer> cityAdCodes);
    }

}
